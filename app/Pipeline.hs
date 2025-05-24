module Pipeline (
    createPipeline,
    Pipeline.createRenderPass,
) where

import qualified Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))

import Control.Monad.Trans.Resource
import Data.Bits
import Data.Foldable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Swapchain
import Vulkan.CStruct.Extends
import Vulkan.Core10 hiding (createRenderPass)
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D (..))
import qualified Vulkan.Core10.FundamentalTypes as Rect2D (Rect2D (..))
import qualified Vulkan.Core10.Pass as AttachmentDescription (AttachmentDescription (..))
import qualified Vulkan.Core10.Pass as AttachmentReference (AttachmentReference (..))
import qualified Vulkan.Core10.Pass as RenderPassCreateInfo (RenderPassCreateInfo (..))
import qualified Vulkan.Core10.Pipeline as PipelineColorBlendStateCreateInfo (PipelineColorBlendStateCreateInfo (..))
import qualified Vulkan.Core10.Pipeline as PipelineRasterizationStateCreateInfo (PipelineRasterizationStateCreateInfo (..))
import qualified Vulkan.Core10.Pipeline as PipelineShaderStageCreateInfo (PipelineShaderStageCreateInfo (..))
import qualified Vulkan.Core10.Pipeline as Viewport (Viewport (..))
import qualified Vulkan.Core10.PipelineLayout as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import Vulkan.Utils.ShaderQQ.GLSL.Shaderc (frag, vert)
import Vulkan.Zero

createPipeline :: (MonadResource m) => Device -> SwapchainInfo -> RenderPass -> m (ReleaseKey, Pipeline)
createPipeline dev swapchainInfo renderPass = do
    (shaderKeys, shaderStages) <- V.unzip <$> createShaders dev
    let dynamicStates = zero{dynamicStates = [DYNAMIC_STATE_VIEWPORT, DYNAMIC_STATE_SCISSOR]}
        vertexInput =
            zero
                { vertexBindingDescriptions = V.empty
                , vertexAttributeDescriptions = V.empty
                }
        inputAssembly =
            zero
                { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                , primitiveRestartEnable = False
                }
        viewport =
            zero
                { Viewport.x = 0.0
                , Viewport.y = 0.0
                , Viewport.width = fromIntegral . Extent2D.width $ siImageExtent swapchainInfo
                , Viewport.height = fromIntegral . Extent2D.height $ siImageExtent swapchainInfo
                , minDepth = 0.0
                , maxDepth = 1.0
                }
        scissor =
            zero
                { Rect2D.offset = Offset2D 0 0
                , Rect2D.extent = siImageExtent swapchainInfo
                }
        viewportState =
            zero
                { viewports = [viewport]
                , scissors = [scissor]
                }
        rasterizationState =
            zero
                { depthClampEnable = False
                , rasterizerDiscardEnable = False
                , polygonMode = POLYGON_MODE_FILL
                , lineWidth = 1.0
                , cullMode = CULL_MODE_BACK_BIT
                , frontFace = FRONT_FACE_CLOCKWISE
                , depthBiasEnable = False
                , depthBiasConstantFactor = 0.0
                , PipelineRasterizationStateCreateInfo.depthBiasClamp = 0.0
                , depthBiasSlopeFactor = 0.0
                }
        multisampling =
            zero
                { sampleShadingEnable = False
                , rasterizationSamples = SAMPLE_COUNT_1_BIT
                , minSampleShading = 1.0
                , sampleMask = V.empty
                , alphaToCoverageEnable = False
                , alphaToOneEnable = False
                }
        colorBlendAttachment =
            zero
                { colorWriteMask = V.foldr1 (.|.) [COLOR_COMPONENT_R_BIT, COLOR_COMPONENT_G_BIT, COLOR_COMPONENT_B_BIT, COLOR_COMPONENT_A_BIT]
                , blendEnable = False
                , srcColorBlendFactor = BLEND_FACTOR_ONE
                , dstColorBlendFactor = BLEND_FACTOR_ZERO
                , colorBlendOp = BLEND_OP_ADD
                , srcAlphaBlendFactor = BLEND_FACTOR_ONE
                , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
                , alphaBlendOp = BLEND_OP_ADD
                }
        colorBlending =
            zero
                { logicOpEnable = False
                , PipelineColorBlendStateCreateInfo.logicOp = LOGIC_OP_COPY
                , PipelineColorBlendStateCreateInfo.attachments = [colorBlendAttachment]
                , blendConstants = (0.0, 0.0, 0.0, 0.0)
                }
        pipelineLayout =
            zero
                { PipelineLayoutCreateInfo.setLayouts = V.empty
                , pushConstantRanges = V.empty
                }
    (pipelineLayoutKey, pipelineLayout) <- withPipelineLayout dev pipelineLayout Nothing allocate
    let pipelineInfo =
            GraphicsPipelineCreateInfo
                { next = ()
                , flags = zero
                , stageCount = 2
                , stages = shaderStages
                , vertexInputState = Just $ SomeStruct vertexInput
                , inputAssemblyState = Just inputAssembly
                , tessellationState = Nothing
                , viewportState = Just $ SomeStruct viewportState
                , rasterizationState = Just $ SomeStruct rasterizationState
                , multisampleState = Just $ SomeStruct multisampling
                , depthStencilState = Just zero
                , colorBlendState = Just $ SomeStruct colorBlending
                , dynamicState = Just dynamicStates
                , layout = pipelineLayout
                , renderPass = renderPass
                , subpass = 0
                , basePipelineHandle = NULL_HANDLE
                , basePipelineIndex = -1
                }
    -- Ignore warning if only using a single pipeline
    (key, (_, ~[graphicsPipelines])) <- withGraphicsPipelines dev NULL_HANDLE [SomeStruct pipelineInfo] Nothing allocate
    release pipelineLayoutKey
    traverse_ release shaderKeys
    return (key, graphicsPipelines)

createRenderPass :: (MonadResource m) => Device -> SwapchainInfo -> m (ReleaseKey, RenderPass)
createRenderPass dev swapchainInfo = do
    let colorAttachment =
            zero
                { AttachmentDescription.format = SurfaceFormatKHR.format $ siSurfaceFormat swapchainInfo
                , AttachmentDescription.samples = SAMPLE_COUNT_1_BIT
                , loadOp = ATTACHMENT_LOAD_OP_CLEAR
                , storeOp = ATTACHMENT_STORE_OP_STORE
                , stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE
                , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
                , AttachmentDescription.initialLayout = IMAGE_LAYOUT_UNDEFINED
                , finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
                }
        colorAttachmentRef =
            zero
                { AttachmentReference.attachment = 0
                , AttachmentReference.layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
        subPass =
            zero
                { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
                , colorAttachments = [colorAttachmentRef]
                }
        renderPassInfo =
            zero
                { RenderPassCreateInfo.attachments = [colorAttachment]
                , subpasses = [subPass]
                }
    (renderPassKey, renderPass) <- withRenderPass dev renderPassInfo Nothing allocate
    return (renderPassKey, renderPass)

createShaders ::
    (MonadResource m) =>
    Device ->
    m (Vector (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo))
createShaders dev = do
    let vertcode =
            [vert|
            #version 450
            vec2 positions[3] = vec2[](
                vec2(0.0, -0.5),
                vec2(-0.5, 0.5),
                vec2(0.5,0.5)
            );

            void main() {
                gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
            }
        |]
        fragcode =
            [frag|
            #version 450
            layout(location = 0) out vec4 outColor;

            void main() {
                outColor = vec4(1.0, 0.0, 0.0, 1.0);
            } 
        |]

    (vertKey, vertModule) <- withShaderModule dev (zero{code = vertcode}) Nothing allocate
    (fragKey, fragModule) <- withShaderModule dev (zero{code = fragcode}) Nothing allocate

    let vertShaderStageCreateInfo =
            zero
                { PipelineShaderStageCreateInfo.stage = SHADER_STAGE_VERTEX_BIT
                , module' = vertModule
                , name = "main"
                }
        fragShaderStageCreateInfo =
            zero
                { PipelineShaderStageCreateInfo.stage = SHADER_STAGE_FRAGMENT_BIT
                , module' = fragModule
                , name = "main"
                }
    pure [(vertKey, SomeStruct vertShaderStageCreateInfo), (fragKey, SomeStruct fragShaderStageCreateInfo)]
