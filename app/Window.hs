module Window where

import Control.Monad
import Control.Monad.Trans.Resource
import Foreign.Marshal (malloc)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek)
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk
import UnliftIO.Exception
import UnliftIO

-- prevent orphan instance warnings
newtype GLFWVkResult = GLFWVkResult Vk.Result

instance Enum GLFWVkResult where
    toEnum = GLFWVkResult . Vk.Result . fromIntegral
    fromEnum (GLFWVkResult (Vk.Result result)) = fromIntegral result

createGLFWWindow ::
    (MonadResource m) =>
    Int ->
    Int ->
    String ->
    Maybe GLFW.Monitor ->
    Maybe GLFW.Window ->
    m (ReleaseKey, GLFW.Window)
createGLFWWindow width height winTitle monitor window = do
    (releaseKey, maybeWindow) <-
        allocate
            (GLFW.createWindow width height winTitle monitor window)
            (mapM_ GLFW.destroyWindow)
    case maybeWindow of
        Just window -> return (releaseKey, window)
        Nothing -> throwString "Failed to create GLFW window"

createSurface :: (MonadResource m) => Vk.Instance -> GLFW.Window -> m (ReleaseKey, Vk.SurfaceKHR)
createSurface inst window = do
    allocate
        ( do
            pSurf <- malloc
            (GLFWVkResult vkres) <- GLFW.createWindowSurface (castPtr (Vk.instanceHandle inst)) window nullPtr pSurf
            unless (vkres == Vk.SUCCESS) $ throwString "Failed to create VkSurfaceKHR"
            -- putStrLn ("VkSurfaceKHR ptr: " ++ show pSurf)
            -- TODO: check pSurf alignment constraint before dereferencing
            peek pSurf
        )
        (\s -> Vk.destroySurfaceKHR inst s Nothing)

-- Fills an IORef with True on window resize
-- On read, value is reset to False
makeWindowSizeCallback :: (MonadIO m) => m (Maybe GLFW.WindowSizeCallback, IO Bool)
makeWindowSizeCallback = do
    windowResizeRef <- newIORef False
    let windowSizeCallback _win _width _height = do
            atomicWriteIORef windowResizeRef True
    -- Set the value back to false on read
    let readWindowSizeRef = do
            result <- readIORef windowResizeRef
            when result $ atomicWriteIORef windowResizeRef False
            return result
    return (Just windowSizeCallback, readWindowSizeRef)
