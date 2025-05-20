module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS

import qualified Init
import Window

import qualified Graphics.UI.GLFW as GLFW

import qualified Vulkan.Core10 as Vk.CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import Vulkan.Zero

main :: IO ()
main = runResourceT $ do
    _ <- allocate_ GLFW.init GLFW.terminate
    liftIO $ do
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 9)
        GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
        GLFW.windowHint (GLFW.WindowHint'Resizable False)

    let winWidth = 800
        winHeight = 600
        winTitle = "My Window"

    (_, window) <- createGLFWWindow winWidth winHeight winTitle Nothing Nothing
    glfwExtensions <- liftIO $ mapM BS.packCString =<< GLFW.getRequiredInstanceExtensions
    liftIO $ GLFW.makeContextCurrent (Just window)

    inst <- Init.createInstance glfwExtensions
    surface <- createSurface inst window
    Init.DeviceParams devName phys dev graphicsQueue graphicsQueueFamilyIndex <-
        Init.createDevice inst (snd surface)
    let commandPoolCreateInfo = zero{Vk.CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex}

    liftIO $
        mainloop window $ do
            GLFW.pollEvents
            GLFW.swapBuffers window

mainloop :: GLFW.Window -> IO () -> IO ()
mainloop window draw = do
    shouldClose <- GLFW.windowShouldClose window
    unless shouldClose $ do
        draw
        mainloop window draw
