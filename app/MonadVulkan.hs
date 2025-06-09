module MonadVulkan (
    GlobalHandles (..),
    RecycledResources (..),
    initGlobalHandles,
    sameGraphicsPresentQueues,
    spawn_,
) where

import Control.Concurrent.Chan.Unagi
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Word
import UnliftIO (Async, asyncWithUnmask, mask, toIO, uninterruptibleCancel)
import Vulkan.Core10

type QueueFamilyIndex = Word32

-- | A bunch of global, unchanging state we cart around
data GlobalHandles = GlobalHandles
    { ghInstance :: Instance
    , ghPhysicalDevice :: PhysicalDevice
    , ghDevice :: Device
    , ghGraphicsQueue :: (Queue, QueueFamilyIndex)
    , ghPresentQueue :: (Queue, QueueFamilyIndex)
    , ghRecycleBin :: RecycledResources -> IO ()
    -- ^ Filled with resources which aren't destroyed after finishing a frame,
    -- but instead are used by another frame which executes after that one is
    -- retired, (taken from ghRecycleNib)
    --
    -- Make sure not to pass any resources which were created with a frame-only
    -- scope however!
    , ghRecycleNib :: IO (Either (IO RecycledResources) RecycledResources)
    -- ^ The resources of prior frames waiting to be taken
    }

sameGraphicsPresentQueues :: GlobalHandles -> Bool
sameGraphicsPresentQueues = (==) <$> (snd . ghGraphicsQueue) <*> (snd . ghPresentQueue)

data RecycledResources = RecycledResources
    { fImageAvailableSemaphore :: Semaphore
    -- ^ A binary semaphore passed to 'acquireNextImageKHR'
    , fRenderFinishedSemaphore :: Semaphore
    -- ^ A binary semaphore to synchronize rendering and presenting
    , fCommandPool :: CommandPool
    -- ^ Pool for this frame's commands (might want more than one of these for
    -- multithreaded recording)
    }

initGlobalHandles ::
    (MonadResource m) =>
    Instance ->
    PhysicalDevice ->
    Device ->
    -- | Graphics Queue
    (Queue, QueueFamilyIndex) ->
    -- | Present Queue
    (Queue, QueueFamilyIndex) ->
    m GlobalHandles
initGlobalHandles ghInstance ghPhysicalDevice ghDevice ghGraphicsQueue ghPresentQueue = do
    (bin, nib) <- liftIO newChan
    let ghRecycleBin = writeChan bin
        ghRecycleNib = do
            (try, block) <- tryReadChan nib
            maybe (Left block) Right <$> tryRead try
    return GlobalHandles{..}

-- Start an async thread which will be cancelled at the end of the ResourceT
-- block
spawn :: (MonadUnliftIO m, MonadResource m) => m a -> m (Async a)
spawn a = do
    aIO <- toIO a
    -- If we don't remove the release key when the thread is done it'll leak,
    -- remove it at the end of the async action when the thread is going to die
    -- anyway.
    --
    -- Mask this so there's no chance we're inturrupted before writing the mvar.
    kv <- liftIO newEmptyMVar
    UnliftIO.mask $ \_ -> do
        (k, r) <-
            allocate
                ( asyncWithUnmask
                    (\unmask -> unmask $ aIO <* (unprotect =<< liftIO (readMVar kv)))
                )
                uninterruptibleCancel
        liftIO $ putMVar kv k
        pure r

spawn_ :: (MonadUnliftIO m, MonadResource m) => m a -> m ()
spawn_ = void . spawn
