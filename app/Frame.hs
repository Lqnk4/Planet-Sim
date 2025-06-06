{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Frame (
    Frame (..),
    RecycledResources (..),
    initialFrame,
) where

import Control.Monad.Trans.Resource
import Data.Foldable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Graphics.UI.GLFW as GLFW
import MonadVulkan
import qualified Pipeline
import RefCounted
import Swapchain
import Vulkan.CStruct.Extends
import Vulkan.Core10 hiding (createCommandPool)
import qualified Vulkan.Core10.CommandPool as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10.Fence as FenceCreateInfo (FenceCreateInfo (..))
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D (..))
import qualified Vulkan.Core10.Pass as FramebufferCreateInfo (FramebufferCreateInfo (..))
import Vulkan.Core12
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Zero

-- maxFramesInFlight :: Int
-- maxFramesInFlight = 2

data Frame = Frame
    { fIndex :: Word64
    , fWindow :: GLFW.Window
    , fSurface :: SurfaceKHR
    , fSwapchainResources :: SwapchainResources
    , fPipeline :: Pipeline
    , fRenderPass :: RenderPass
    , fFramebuffers :: Vector Framebuffer
    , fReleaseFramebuffers :: RefCounted
    , fRecycledResources :: RecycledResources
    }

initialRecycledResources :: (MonadResource m) => GlobalHandles -> m RecycledResources
initialRecycledResources gh@GlobalHandles{..} = do
    (_, fImageAvailableSemaphore) <-
        withSemaphore ghDevice (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ()) Nothing allocate
    (_, fRenderFinishedSemaphore) <-
        withSemaphore ghDevice (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ()) Nothing allocate
    (_, fInFlightFence) <-
        withFence ghDevice (zero{FenceCreateInfo.flags = FENCE_CREATE_SIGNALED_BIT} ::& ()) Nothing allocate
    (_, fCommandPool) <- createCommandPool gh
    return RecycledResources{..}

initialFrame :: (MonadResource m) => GlobalHandles -> GLFW.Window -> SurfaceKHR -> m Frame
initialFrame gh@GlobalHandles{..} fWindow fSurface = do
    let fIndex = 1
        oldSwapchain = NULL_HANDLE
    fSwapchainResources <- allocSwapchainResources gh oldSwapchain fWindow fSurface
    (_, fRenderPass) <- Pipeline.createRenderPass ghDevice (srInfo fSwapchainResources)
    (fReleaseFramebuffers, fFramebuffers) <- createFramebuffers gh fRenderPass fSwapchainResources
    (_, fPipeline) <- Pipeline.createPipeline ghDevice (srInfo fSwapchainResources) fRenderPass
    fRecycledResources <- initialRecycledResources gh
    return Frame{..}

createCommandPool :: (MonadResource m) => GlobalHandles -> m (ReleaseKey, CommandPool)
createCommandPool GlobalHandles{..} = do
    let poolInfo =
            zero
                { CommandPoolCreateInfo.flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                , CommandPoolCreateInfo.queueFamilyIndex = snd ghGraphicsQueue
                }
    withCommandPool ghDevice poolInfo Nothing allocate

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
