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
import Init (DeviceParams (..), sameGraphicsPresentQueues)
import Vulkan.Core10
import qualified Vulkan.Core10 as ImageViewCreateInfo (ImageViewCreateInfo (..))
import Vulkan.Exception ()
import Vulkan.Extensions.VK_KHR_surface
import qualified Vulkan.Extensions.VK_KHR_surface as SurfaceCapabilitiesKHR (SurfaceCapabilitiesKHR (..))
import qualified Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero

data SwapchainInfo = SwapchainInfo
    { siSwapchain :: SwapchainKHR
    , siSwapchainReleaseKey :: ReleaseKey
    , siPresentMode :: PresentModeKHR
    , siSurfaceFormat :: SurfaceFormatKHR
    , siImageExtent :: Extent2D
    , siSurface :: SurfaceKHR
    , siImageViews :: Vector (ReleaseKey, ImageView)
    }

createSwapchain ::
    (MonadResource m) =>
    -- | old swapchain, can be NULL_HANDLE
    SwapchainKHR ->
    -- | extent to use if swapchain size determines surface size
    -- ^ Use GLFW.getFrameBufferSize
    DeviceParams ->
    Extent2D ->
    SurfaceKHR ->
    m SwapchainInfo
createSwapchain oldSwapchain devParams explicitSize surf = do
    let phys = dpPhysicalDevice devParams
    surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR phys surf
    (_, availableFormats) <- getPhysicalDeviceSurfaceFormatsKHR phys surf
    (_, availablePresentModes) <- getPhysicalDeviceSurfacePresentModesKHR phys surf

    surfaceFormat <- chooseSurfaceFormat availableFormats

    let desiredPresentModes = [PRESENT_MODE_MAILBOX_KHR]
    let requiredUsageFlags = [IMAGE_USAGE_COLOR_ATTACHMENT_BIT] :: [ImageUsageFlagBits]

    presentMode <- case filter (`V.elem` availablePresentModes) desiredPresentModes of
        [] -> pure PRESENT_MODE_FIFO_KHR
        (x : _) -> pure x

    let imageExtent = case currentExtent (surfaceCaps :: SurfaceCapabilitiesKHR) of
            Extent2D w h | w == maxBound, h == maxBound -> explicitSize
            e -> e

    let imageCount =
            let
                limit = case maxImageCount (surfaceCaps :: SurfaceCapabilitiesKHR) of
                    0 -> maxBound
                    n -> n
                -- Request one additional image to prevent us having to wait for
                -- the driver to finish
                buffer = 1
                desired =
                    buffer + SurfaceCapabilitiesKHR.minImageCount surfaceCaps
             in
                min limit desired

    let (imageSharingMode, queueFamilyIndices) =
            if sameGraphicsPresentQueues devParams
                then (SHARING_MODE_EXCLUSIVE, [dpGraphicsQueueFamilyIndex devParams])
                else (SHARING_MODE_CONCURRENT, [dpGraphicsQueueFamilyIndex devParams, dpPresentQueueFamilyIndex devParams])

    let swapchainCreateInfo =
            SwapchainCreateInfoKHR
                { next = ()
                , flags = zero
                , surface = surf
                , minImageCount = imageCount
                , imageFormat = SurfaceFormatKHR.format surfaceFormat
                , imageColorSpace = colorSpace surfaceFormat
                , imageExtent = imageExtent
                , imageArrayLayers = 1
                , imageUsage = foldr (.|.) zero requiredUsageFlags
                , imageSharingMode = imageSharingMode
                , queueFamilyIndices = queueFamilyIndices
                , preTransform = currentTransform surfaceCaps
                , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                , presentMode = presentMode
                , clipped = True
                , oldSwapchain = oldSwapchain
                }

    (releaseKey, swapchain) <- withSwapchainKHR (dpDevice devParams) swapchainCreateInfo Nothing allocate

    (_, images) <- getSwapchainImagesKHR (dpDevice devParams) swapchain
    imageViews <- V.forM images $ \image -> do
                let imageCreateInfo image = zero
                            { ImageViewCreateInfo.image = image
                            , viewType = IMAGE_VIEW_TYPE_2D
                            , ImageViewCreateInfo.format = SurfaceFormatKHR.format surfaceFormat
                            , components =
                                ComponentMapping
                                    { r = COMPONENT_SWIZZLE_IDENTITY
                                    , g = COMPONENT_SWIZZLE_IDENTITY
                                    , b = COMPONENT_SWIZZLE_IDENTITY
                                    , a = COMPONENT_SWIZZLE_IDENTITY
                                    }
                            , ImageViewCreateInfo.subresourceRange =
                                ImageSubresourceRange
                                    { aspectMask = IMAGE_ASPECT_COLOR_BIT
                                    , baseMipLevel = 0
                                    , levelCount = 1
                                    , baseArrayLayer = 0
                                    , layerCount = 1
                                    }
                            }
                withImageView (dpDevice devParams) (imageCreateInfo image) Nothing allocate



    return $ SwapchainInfo swapchain releaseKey presentMode surfaceFormat imageExtent surf imageViews

chooseSurfaceFormat :: (MonadResource m) => Vector SurfaceFormatKHR -> m SurfaceFormatKHR
chooseSurfaceFormat formats = do
    let best = V.find (\surfaceFormat -> SurfaceFormatKHR.format surfaceFormat == FORMAT_B8G8R8A8_SRGB && colorSpace surfaceFormat == COLOR_SPACE_SRGB_NONLINEAR_KHR) formats
    return $ case best of
        (Just surfaceFormat) -> surfaceFormat
        Nothing -> throw $ AssertionFailed "Could not find suitable surface format"


--
-- Utils
--

-- infixl 4 .&&.
-- (.&&.) :: (Bits a) => a -> a -> Bool
-- x .&&. y = (/= zeroBits) (x .&. y)
