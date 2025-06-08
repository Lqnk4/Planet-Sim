{-# LANGUAGE NumDecimals #-}

module Render (
    renderFrame,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Vector as V
import Data.Word
import Frame
import MonadVulkan
import Swapchain
import UnliftIO.Exception
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Core12
import qualified Vulkan.Core10.CommandBuffer as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10.CommandBufferBuilding as RenderPassBeginInfo (RenderPassBeginInfo (..))
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D (..))
import qualified Vulkan.Core10.Queue as SubmitInfo (SubmitInfo (..))
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_swapchain
import qualified Vulkan.Extensions.VK_KHR_swapchain as PresentInfoKHR (PresentInfoKHR (..))
import Vulkan.Zero

renderFrame :: GlobalHandles -> F ()
renderFrame GlobalHandles{..} = do
    f@Frame{..} <- askFrame
    let RecycledResources{..} = fRecycledResources
        oneSecond = 1.0e9 :: Word64
        SwapchainResources{..} = fSwapchainResources
        SwapchainInfo{..} = srInfo

    -- Ensure the swapchain survives for the duration of the frame
    frameRefCount srRelease
    frameRefCount fReleaseFramebuffers

    imageIndex <-
        acquireNextImageKHRSafe ghDevice siSwapchain oneSecond fImageAvailableSemaphore NULL_HANDLE
            >>= \case
                (SUCCESS, imageIndex) -> return imageIndex
                (SUBOPTIMAL_KHR, imageIndex) -> return imageIndex
                (TIMEOUT, _) ->
                    timeoutError $ "Timed out (" ++ show (oneSecond `div` 1.0e9) ++ "s) trying to aquire next Image"
                (e@ERROR_OUT_OF_DATE_KHR, _) ->
                    throwIO $ VulkanException e
                (e, _) -> throwString $ "Unexpected Result " ++ show e ++ "  from acquireNextImageKHR"

    let commandBufferAllocateInfo =
            zero
                { commandPool = fCommandPool
                , level = COMMAND_BUFFER_LEVEL_PRIMARY
                , commandBufferCount = 1
                }
    (_, commandBuffers) <- withCommandBuffers ghDevice commandBufferAllocateInfo allocate
    V.mapM_ (`resetCommandBuffer` zero) commandBuffers -- May be redundant, commandBuffer must be in the 'initial' state before recording
    let commandBufferBeginInfo =
            zero
                { CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                , inheritanceInfo = Nothing
                }
    V.forM_
        commandBuffers
        ( \commandBuffer ->
            useCommandBuffer commandBuffer commandBufferBeginInfo $
                myRecordCommandBuffer f imageIndex commandBuffer
        )

    let submitInfo =
            zero
                { SubmitInfo.waitSemaphores = [fImageAvailableSemaphore]
                , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                , commandBuffers = V.map commandBufferHandle commandBuffers
                , SubmitInfo.signalSemaphores = [fRenderFinishedSemaphore, fRenderFinishedHostSemaphore]
                }
                ::& zero
                    { waitSemaphoreValues = [1]
                    , signalSemaphoreValues = [1, fIndex]
                    }
                :& ()
    queueSubmitFrame (fst ghGraphicsQueue) [SomeStruct submitInfo] fRenderFinishedHostSemaphore fIndex

    let presentInfo =
            zero
                { PresentInfoKHR.waitSemaphores = [fRenderFinishedSemaphore]
                , swapchains = [siSwapchain]
                , imageIndices = [imageIndex]
                }
    -- present the frame when the render is finished
    void $ queuePresentKHR (fst ghPresentQueue) presentInfo

myRecordCommandBuffer :: (MonadIO m) => Frame -> Word32 -> CommandBuffer -> m ()
myRecordCommandBuffer Frame{..} imageIndex commandBuffer = do
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
