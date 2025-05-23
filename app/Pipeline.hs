module Pipeline where

import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import Vulkan.CStruct.Extends
import Vulkan.Core10
import qualified Vulkan.Core10.Pipeline as PipelineShaderStageCreateInfo (PipelineShaderStageCreateInfo (..))
import Vulkan.Utils.ShaderQQ.GLSL.Shaderc (frag, vert)
import Vulkan.Zero

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
