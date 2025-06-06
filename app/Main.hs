module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS

import qualified Init
import Window

import Frame (initialFrame)
import qualified Graphics.UI.GLFW as GLFW
import MonadVulkan
import Render (renderFrame)
import Vulkan.Core10

main :: IO ()
main = runResourceT $ do
    _ <- allocate_ GLFW.init GLFW.terminate
    liftIO $ do
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 9)
        GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
        GLFW.windowHint (GLFW.WindowHint'Resizable False)

    let winWidth = 800
        winHeight = 800
        winTitle = "My Window"

    (_, window) <- createGLFWWindow winWidth winHeight winTitle Nothing Nothing
    glfwExtensions <- liftIO $ mapM BS.packCString =<< GLFW.getRequiredInstanceExtensions
    liftIO $ GLFW.makeContextCurrent (Just window)

    inst <- Init.createInstance glfwExtensions
    (_, surface) <- createSurface inst window
    devParams@Init.DeviceParams{..} <- Init.createDevice inst surface
    globalHandles <- initGlobalHandles inst dpPhysicalDevice dpDevice dpGraphicsQueue dpPresentQueue

    firstFrame <- initialFrame globalHandles window surface

    mainloop window $ do
        liftIO GLFW.pollEvents
        liftIO $ GLFW.swapBuffers window
        renderFrame devParams firstFrame
    deviceWaitIdle (Init.dpDevice devParams)

mainloop :: (MonadIO m) => GLFW.Window -> m () -> m ()
mainloop window draw = do
    shouldClose <- liftIO $ GLFW.windowShouldClose window
    unless shouldClose $ do
        draw
        mainloop window draw
