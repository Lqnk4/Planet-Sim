module Swapchain where

import Control.Monad.Trans.Resource
import MonadVulkan
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk

createSwapchain :: Vk.SwapchainKHR -> Vk.Extent2D -> Vk.SurfaceKHR -> V (ReleaseKey, Vk.SwapchainKHR, Vk.SurfaceFormatKHR, Vk.Extent2D)
createSwapchain oldSwapChain explicitSize surf = do
    undefined
