{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumDecimals #-}

module Frame (
    Frame (..),
    F (..),
    askFrame,
    asksFrame,
    frameRefCount,
    queueSubmitFrame,
    RecycledResources (..),
    initialFrame,
    advanceFrame,
    runFrame,
    timeoutError,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Foldable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.IO.Exception (IOErrorType (TimeExpired), IOException (..))
import qualified Graphics.UI.GLFW as GLFW
import MonadVulkan
import qualified Pipeline
import RefCounted
import Swapchain
import UnliftIO
import Vulkan.CStruct.Extends
import Vulkan.Core10 
import qualified Vulkan.Core10.CommandPool as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D (..))
import qualified Vulkan.Core10.Pass as FramebufferCreateInfo (FramebufferCreateInfo (..))
import Vulkan.Core12
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Zero

maxFramesInFlight :: Int
maxFramesInFlight = 2

data Frame = Frame
    { fIndex :: Word64
    , fWindow :: GLFW.Window
    , fSurface :: SurfaceKHR
    , fSwapchainResources :: SwapchainResources
    , fPipeline :: Pipeline
    , fRenderPass :: RenderPass
    , fFramebuffers :: Vector Framebuffer
    , fReleaseFramebuffers :: RefCounted
    , fRenderFinishedHostSemaphore :: Semaphore
    -- ^ timeline semaphore which increments to fIndex when this frame is done
    -- the host can wait on this semaphore
    , fRecycledResources :: RecycledResources
    -- ^ Resources which are passed through a channel to the next frame
    , fWorkProgress :: IORef [(Semaphore, Word64)]
    -- ^ Timeline semaphores and corresponding wait values, updates as the frame progresses
    , fResources :: (ReleaseKey, InternalState)
    -- turns frame into a MonadResource, used for tracking frame local resources
    }

newtype F a = F {unF :: ReaderT Frame (ResourceT IO) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadFail
        , MonadIO
        )

instance MonadUnliftIO F where
    withRunInIO a = F $ withRunInIO (\r -> a (r . unF))

instance MonadResource F where
    liftResourceT r = do
        i <- asksFrame (snd . fResources)
        liftIO $ runInternalState r i

askFrame :: F Frame
askFrame = F ask

asksFrame :: (Frame -> a) -> F a
asksFrame = F . asks

frameRefCount :: RefCounted -> F ()
frameRefCount = resourceTRefCount

queueSubmitFrame :: Queue -> Vector (SomeStruct SubmitInfo) -> Semaphore -> Word64 -> F ()
queueSubmitFrame q ss sem value = do
    gpuWork <- asksFrame fWorkProgress
    -- mask to not get interrupted between submitting the work and recording the wait
    mask $ \_ -> do
        queueSubmit q ss NULL_HANDLE
        atomicModifyIORef' gpuWork ((, ()) . ((sem, value) :))

-- Runs a frame computation and spanws a thread to wait for GPU work to complete,
-- then collects frame specific resources
runFrame :: GlobalHandles -> Frame -> F a -> ResourceT IO a
runFrame gh@GlobalHandles{..} f@Frame{..} (F r) =
    runReaderT r f `finally` do
        waits <- liftIO $ readIORef fWorkProgress
        let oneSecond = 1.0e9
        spawn_ $ do
            unless (null waits) $ do
                let waitInfo =
                        zero
                            { semaphores = V.fromList (fst <$> waits)
                            , values = V.fromList (snd <$> waits)
                            }
                waitTwice gh waitInfo oneSecond >>= \case
                    TIMEOUT ->
                        timeoutError $ "Timeed out (" ++ show (oneSecond `div` 1.0e9) ++ "s) waiting for frame to finish on Device"
                    _ -> return ()
                resetCommandPool ghDevice (fCommandPool fRecycledResources) COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
                liftIO $ ghRecycleBin fRecycledResources

                release (fst fResources)

initialRecycledResources :: (MonadResource m) => GlobalHandles -> m RecycledResources
initialRecycledResources GlobalHandles{..} = do
    (_, fImageAvailableSemaphore) <-
        withSemaphore ghDevice (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ()) Nothing allocate
    (_, fRenderFinishedSemaphore) <-
        withSemaphore ghDevice (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ()) Nothing allocate
    let poolInfo =
            zero
                { CommandPoolCreateInfo.queueFamilyIndex = snd ghGraphicsQueue
                , CommandPoolCreateInfo.flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                }
    (_, fCommandPool) <- withCommandPool ghDevice poolInfo Nothing allocate
    return RecycledResources{..}

initialFrame :: (MonadResource m) => GlobalHandles -> GLFW.Window -> SurfaceKHR -> m Frame
initialFrame gh@GlobalHandles{..} fWindow fSurface = do
    let fIndex = 1
        oldSwapchain = NULL_HANDLE
    fSwapchainResources <- allocSwapchainResources gh oldSwapchain fWindow fSurface
    (_, fRenderPass) <- Pipeline.createRenderPass ghDevice (srInfo fSwapchainResources)
    (fReleaseFramebuffers, fFramebuffers) <- createFramebuffers gh fRenderPass fSwapchainResources
    (_releasePipeline, fPipeline) <- Pipeline.createPipeline ghDevice (srInfo fSwapchainResources) fRenderPass
    (_, fRenderFinishedHostSemaphore) <-
        withSemaphore
            ghDevice
            (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_TIMELINE 0 :& ())
            Nothing
            allocate
    replicateM_ (maxFramesInFlight - 1) $ liftIO . ghRecycleBin =<< initialRecycledResources gh
    fRecycledResources <- initialRecycledResources gh

    fWorkProgress <- liftIO $ newIORef []

    fResources <- allocate createInternalState closeInternalState

    return Frame{..}

advanceFrame :: (MonadResource m, MonadIO m, MonadThrow m) => GlobalHandles -> Bool -> Frame -> m Frame
advanceFrame gh needsNewSwapchain f = do
    -- steal resources from prior frame
    let nib = ghRecycleNib gh
    fRecycledResources <-
        liftIO $
            nib >>= \case
                Left block -> block
                Right rs -> pure rs
    (fSwapchainResources, fFramebuffers, fReleaseFramebuffers) <-
        if needsNewSwapchain
            then do
                fbSize <- liftIO $ GLFW.getFramebufferSize (fWindow f)
                let handleMinimization (width, height) = do
                        when (width == 0 && height == 0) $ do
                            (width', height') <- GLFW.getFramebufferSize (fWindow f)
                            handleMinimization (width', height')
                liftIO $ handleMinimization fbSize

                deviceWaitIdle (ghDevice gh)
                swapchainResources <- recreateSwapchainResources gh (fWindow f) (fSwapchainResources f)
                liftIO $ putStrLn "Recreated Swapchain"
                unless
                    (siSurfaceFormat (srInfo swapchainResources) == siSurfaceFormat (srInfo (fSwapchainResources f)))
                    $ throwString "TODO: Handle swapchain changing formats"
                releaseRefCounted (fReleaseFramebuffers f)
                (releaseFramebuffers, framebuffers) <- createFramebuffers gh (fRenderPass f) swapchainResources
                return (swapchainResources, framebuffers, releaseFramebuffers)
            else return (fSwapchainResources f, fFramebuffers f, fReleaseFramebuffers f)

    fWorkProgress <- liftIO $ newIORef []
    fResources <- allocate createInternalState closeInternalState

    return
        Frame
            { fIndex = succ (fIndex f)
            , fWindow = fWindow f
            , fSurface = fSurface f
            , fSwapchainResources
            , fPipeline = fPipeline f
            , fRenderFinishedHostSemaphore = fRenderFinishedHostSemaphore f
            , fRenderPass = fRenderPass f
            , fFramebuffers
            , fReleaseFramebuffers
            , fWorkProgress
            , fResources
            , fRecycledResources
            }

createFramebuffers :: (MonadResource m) => GlobalHandles -> RenderPass -> SwapchainResources -> m (RefCounted, Vector Framebuffer)
createFramebuffers gh renderPass SwapchainResources{..} = do
    let SwapchainInfo{..} = srInfo
        framebufferInfo imageView =
            zero
                { FramebufferCreateInfo.renderPass = renderPass
                , FramebufferCreateInfo.attachments = [imageView]
                , FramebufferCreateInfo.width = Extent2D.width siImageExtent
                , FramebufferCreateInfo.height = Extent2D.height siImageExtent
                , layers = 1
                }
    (framebufferKeys, frameBuffers) <- fmap V.unzip . V.forM srImageViews $ \imageView ->
        withFramebuffer (ghDevice gh) (framebufferInfo imageView) Nothing allocate
    releaseFramebuffers <- newRefCounted (traverse_ release framebufferKeys)
    return (releaseFramebuffers, frameBuffers)

waitTwice :: (MonadIO m) => GlobalHandles -> SemaphoreWaitInfo -> Word64 -> m Result
waitTwice gh waitInfo t =
    waitSemaphoresSafe (ghDevice gh) waitInfo t >>= \case
        TIMEOUT -> waitSemaphoresSafe (ghDevice gh) waitInfo 0
        r -> return r

timeoutError :: (MonadIO m) => String -> m a
timeoutError message = liftIO . throwIO $ IOError Nothing TimeExpired "" message Nothing Nothing
