module Swapchain (
    createSwapchain,
    SwapchainInfo (..),
) where

import Control.Exception
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Either ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.Core10
import Vulkan.Exception ()
import Vulkan.Extensions.VK_KHR_surface
import qualified Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Extensions.VK_KHR_swapchain

data SwapchainInfo = SwapchainInfo
    { siSwapchain :: SwapchainKHR
    , siSwapchainReleaseKey :: ReleaseKey
    , siPresentMode :: PresentModeKHR
    , siSurfaceFormat :: SurfaceFormatKHR
    , siImageExtent :: Extent2D
    , siSurface :: SurfaceKHR
    }

createSwapchain ::
    (MonadResource m) =>
    PhysicalDevice ->
    -- | extent to use if swapchain size determines window size
    -- ^ Use GLFW.getFrameBufferSize
    Extent2D ->
    SurfaceKHR ->
    m SwapchainInfo
createSwapchain phys explicitSize surf = do
    surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR phys surf
    (_, availableFormats) <- getPhysicalDeviceSurfaceFormatsKHR phys surf
    (_, availablePresentModes) <- getPhysicalDeviceSurfacePresentModesKHR phys surf

    format <- chooseSurfaceFormat availableFormats

    let desiredPresentModes = [PRESENT_MODE_MAILBOX_KHR]
    let requiredUsageFlags = [IMAGE_USAGE_COLOR_ATTACHMENT_BIT, IMAGE_USAGE_STORAGE_BIT]

    presentMode <- case filter (`V.elem` availablePresentModes) desiredPresentModes of
        [] -> pure PRESENT_MODE_FIFO_KHR
        (x : _) -> pure x

    let imageExtent = case currentExtent (surfaceCaps :: SurfaceCapabilitiesKHR) of
            Extent2D w h | w == maxBound, h == maxBound -> explicitSize
            e -> e


    let swapchainCreateInfo =
            SwapchainCreateInfoKHR
                { next = ()
                , flags = zero
                , surface = surf
                , minImageCount = SurfaceCapabilitiesKHR.minImageCount surfaceCaps + 1
                , imageFormat = format
                , imageColorSpace = colorSpace format
                , imageExtent = imageExtent
                , imageArrayLayers = 1
                , imageUsage = foldr (.|.) zero requiredUsageFlags
                , imageSharingMode = _b
                , queueFamilyIndices = _c -- TODO: separate graphicsQueue and presentQueue in Init.hs
                , preTransform = currentTransform surfaceCaps
                }

    _a

chooseSurfaceFormat :: (MonadResource m) => Vector SurfaceFormatKHR -> m SurfaceFormatKHR
chooseSurfaceFormat formats = do
    let best = V.find (\surfaceFormat -> SurfaceFormatKHR.format surfaceFormat == FORMAT_B8G8R8A8_SRGB && colorSpace surfaceFormat == COLOR_SPACE_SRGB_NONLINEAR_KHR) formats
    return $ case best of
        (Just surfaceFormat) -> surfaceFormat
        Nothing -> throw $ AssertionFailed "Could not find suitable surface format"

infixl 4 .&&.
(.&&.) :: (Bits a) => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
