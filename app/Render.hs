{-# LANGUAGE NumDecimals #-}

module Render (
    renderFrame,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Vector as V
import Data.Word
import Frame
import Init
import RefCounted
import Swapchain
import Vulkan.CStruct.Extends
import Vulkan.Core10
import qualified Vulkan.Core10.CommandBuffer as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10.CommandBufferBuilding as RenderPassBeginInfo (RenderPassBeginInfo (..))
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D (..))
import qualified Vulkan.Core10.Queue as SubmitInfo (SubmitInfo (..))
import Vulkan.Extensions.VK_KHR_swapchain
import qualified Vulkan.Extensions.VK_KHR_swapchain as PresentInfoKHR (PresentInfoKHR (..))
import Vulkan.Zero
import Control.Monad

renderFrame :: (MonadResource m) => DeviceParams -> Frame -> m ()
renderFrame DeviceParams{..} f@Frame{..} = do
    let RecycledResources{..} = fRecycledResources
        oneSecond = 1.0e9 :: Word64
        SwapchainResources{..} = fSwapchainResources
        SwapchainInfo{..} = srInfo

    -- Wait for previous frame to finish
    _ <- waitForFences dpDevice [fInFlightFence] True maxBound
    resetFences dpDevice [fInFlightFence]

    -- Ensure the swapchain survives for the duration of the frame
    resourceTRefCount srRelease
    resourceTRefCount fReleaseFramebuffers

    (_, imageIndex) <- acquireNextImageKHRSafe dpDevice siSwapchain oneSecond fImageAvailableSemaphore NULL_HANDLE

    let commandBufferAllocateInfo =
            zero
                { commandPool = fCommandPool
                , level = COMMAND_BUFFER_LEVEL_PRIMARY
                , commandBufferCount = 1
                }
    (_, commandBuffers) <- withCommandBuffers dpDevice commandBufferAllocateInfo allocate
    V.mapM_ (`resetCommandBuffer` zero) commandBuffers -- May be redundant, commandBuffer must be in the 'initial' state before recording
    let commandBufferBeginInfo =
            zero
                { CommandBufferBeginInfo.flags = zero
                , inheritanceInfo = Nothing
                }

    V.forM_ commandBuffers (\commandBuffer -> useCommandBuffer commandBuffer commandBufferBeginInfo $
        myRecordCommandBuffer commandBuffer f imageIndex)

    let signalSemaphores = [fRenderFinishedSemaphore]
        waitSemaphores = [fImageAvailableSemaphore]
        submitInfo =
            zero
                { SubmitInfo.waitSemaphores = waitSemaphores
                , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                , commandBuffers = V.map commandBufferHandle commandBuffers
                , SubmitInfo.signalSemaphores = signalSemaphores
                }
                ::& ()
    queueSubmit (fst dpGraphicsQueue) [SomeStruct submitInfo] fInFlightFence

    let presentInfo =
            zero
                { PresentInfoKHR.waitSemaphores = signalSemaphores
                , swapchains = [siSwapchain]
                , imageIndices = [imageIndex]
                -- , results = _
                }
    -- present the frame when the render is finished
    void $ queuePresentKHR (fst dpPresentQueue) presentInfo

myRecordCommandBuffer :: (MonadIO m) => CommandBuffer -> Frame -> Word32 -> m ()
myRecordCommandBuffer commandBuffer Frame{..} imageIndex = do
    let SwapchainResources{..} = fSwapchainResources
        SwapchainInfo{..} = srInfo
        renderPassBeginInfo =
            zero
                { RenderPassBeginInfo.renderPass = fRenderPass
                , RenderPassBeginInfo.framebuffer = fFramebuffers V.! fromIntegral imageIndex
                , renderArea = Rect2D{offset = zero, extent = siImageExtent}
                , clearValues = [Color (Float32 0.0 0.0 0.0 1.0)]
                }
    cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
        cmdSetViewport
            commandBuffer
            0
            [ Viewport
                { x = 0.0
                , y = 0.0
                , width = realToFrac $ Extent2D.width siImageExtent
                , height = realToFrac $ Extent2D.height siImageExtent
                , minDepth = 0.0
                , maxDepth = 1.0
                }
            ]
        cmdSetScissor
            commandBuffer
            0
            [Rect2D{offset = Offset2D 0 0, extent = siImageExtent}]
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS fPipeline
        cmdDraw commandBuffer 3 1 0 0
