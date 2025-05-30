{-# LANGUAGE NumDecimals #-}

module Render where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Vector as V
import Data.Word
import Frame
import Init
import Swapchain
import Vulkan.Core10
import qualified Vulkan.Core10.CommandBuffer as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10.CommandBufferBuilding as RenderPassBeginInfo (RenderPassBeginInfo (..))
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D (..))
import Vulkan.Zero

renderFrame :: (MonadResource m) => DeviceParams -> Frame -> m ()
renderFrame DeviceParams{..} f@Frame{..} = do
    let RecycledResources{..} = fRecycledResources
        oneSecond = 1.0e9 :: Word64
        SwapchainResources{..} = fSwapchainResources
        SwapchainInfo{..} = srInfo

    -- TODO: set up semaphores
    imageIndex <- undefined

    let commandBufferAllocateInfo =
            zero
                { commandPool = fCommandPool
                , level = COMMAND_BUFFER_LEVEL_PRIMARY
                , commandBufferCount = 1
                }
    (_, ~[commandBuffer]) <- withCommandBuffers dpDevice commandBufferAllocateInfo allocate

    let commandBufferBeginInfo =
            zero
                { CommandBufferBeginInfo.flags = zero
                , inheritanceInfo = Nothing
                }

    useCommandBuffer commandBuffer commandBufferBeginInfo $
        myRecordCommandBuffer commandBuffer f imageIndex

    undefined

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
        cmdDraw commandBuffer 3 1 0 0
