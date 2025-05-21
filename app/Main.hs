module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS

import qualified Init
import Window

import qualified Graphics.UI.GLFW as GLFW

import Swapchain
import Vulkan.Core10.APIConstants
import Vulkan.Core10.FundamentalTypes (Extent2D(..))
import Data.Bifunctor (Bifunctor(bimap))

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
    (fbWidth, fbHeight) <- liftIO $ bimap fromIntegral fromIntegral <$> GLFW.getFramebufferSize window
    glfwExtensions <- liftIO $ mapM BS.packCString =<< GLFW.getRequiredInstanceExtensions
    liftIO $ GLFW.makeContextCurrent (Just window)

    inst <- Init.createInstance glfwExtensions
    (_, surface) <- createSurface inst window
    devParams@(Init.DeviceParams devName phys dev graphicsQueue graphicsQueueFamilyIndex presentQueue presentQueueFamilyIndex) <-
        Init.createDevice inst surface
    swapchainInfo <- createSwapchain NULL_HANDLE devParams (Extent2D fbWidth fbHeight) surface

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
