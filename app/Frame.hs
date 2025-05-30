module Frame where

import Control.Monad.Trans.Resource
import Data.Foldable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Graphics.UI.GLFW as GLFW
import Init
import RefCounted
import Swapchain
import Vulkan.Core10
import qualified Vulkan.Core10.CommandPool as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D (..))
import qualified Vulkan.Core10.Pass as FramebufferCreateInfo (FramebufferCreateInfo (..))
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Zero

numConcurrentFrames :: Int
numConcurrentFrames = 3

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

data RecycledResources = RecycledResources
    { fImageAvailableSemaphore :: Semaphore
    , fRenderFinishedSemaphore :: Semaphore
    , fCommandPool :: CommandPool
    }

createCommandPool :: (MonadResource m) => DeviceParams -> m (ReleaseKey, CommandPool)
createCommandPool devParams = do
    let poolInfo =
            zero
                { CommandPoolCreateInfo.flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                , CommandPoolCreateInfo.queueFamilyIndex = dpGraphicsQueueFamilyIndex devParams
                }
    withCommandPool (dpDevice devParams) poolInfo Nothing allocate

createFramebuffers :: (MonadResource m) => DeviceParams -> RenderPass -> SwapchainResources -> m (RefCounted, Vector Framebuffer)
createFramebuffers devParams renderPass SwapchainResources{..} = do
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
        withFramebuffer (dpDevice devParams) (framebufferInfo imageView) Nothing allocate
    releaseFramebuffers <- newRefCounted (traverse_ release framebufferKeys)
    return (releaseFramebuffers, frameBuffers)
