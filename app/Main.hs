module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS

import qualified Init
import Window

import Data.Maybe
import Frame
import qualified Graphics.UI.GLFW as GLFW
import MonadVulkan
import Render
import Swapchain
import Vulkan.Core10.Queue

main :: IO ()
main = runResourceT $ do
    _ <- allocate_ GLFW.init GLFW.terminate
    liftIO $ do
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 9)
        GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
        GLFW.windowHint (GLFW.WindowHint'Resizable False)
    glfwExtensions <- liftIO $ mapM BS.packCString =<< GLFW.getRequiredInstanceExtensions

    let winWidth = 800
        winHeight = 800
        winTitle = "My Window"

    (_, window) <- createGLFWWindow winWidth winHeight winTitle Nothing Nothing
    liftIO $ GLFW.makeContextCurrent (Just window)

    -- (windowSizeCallback, windowSizeRef) <- makeWindowSizeCallback
    -- liftIO $ GLFW.setWindowSizeCallback window windowSizeCallback

    inst <- Init.createInstance glfwExtensions
    (_, surface) <- createSurface inst window
    Init.DeviceParams{..} <- Init.createDevice inst surface
    globalHandles <- initGlobalHandles inst dpPhysicalDevice dpDevice dpGraphicsQueue dpPresentQueue

    startTime <- liftIO $ fromJust <$> GLFW.getTime
    let reportFPS f = do
            endTime <- liftIO $ fromJust <$> GLFW.getTime
            let frames = fIndex f
                mean = realToFrac frames / (endTime - startTime)
            liftIO $ putStrLn $ "Average FPS: " ++ show mean

    let frame :: Frame -> ResourceT IO (Maybe Frame)
        frame f =
            liftIO (GLFW.windowShouldClose window) >>= \case
                True -> return Nothing
                False ->
                    fmap Just $ do
                        liftIO GLFW.pollEvents
                        liftIO $ GLFW.swapBuffers window
                        -- reportFPS f
                        needsNewSwapchain <- threwSwapchainError $ runFrame globalHandles f (renderFrame globalHandles)
                        advanceFrame globalHandles needsNewSwapchain f

    initial <- initialFrame globalHandles window surface
    mainLoop frame initial
    deviceWaitIdle (ghDevice globalHandles) -- Wait to cleanup resources

mainLoop :: (Monad m) => (a -> m (Maybe a)) -> a -> m ()
mainLoop f x =
    f x >>= \case
        Nothing -> return ()
        Just x' -> mainLoop f x'
