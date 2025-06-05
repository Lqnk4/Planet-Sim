{-# LANGUAGE DeriveGeneric #-}

module Swapchain (
    SwapchainInfo (..),
    SwapchainResources (..),
    allocSwapchainResources,
    recreateSwapchainResources
) where

import Control.Exception
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Either ()
import Data.Foldable
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic (..))
import qualified Graphics.UI.GLFW as GLFW
import Init
import RefCounted
import Vulkan.Core10
import qualified Vulkan.Core10 as ImageViewCreateInfo (ImageViewCreateInfo (..))
import Vulkan.Exception ()
import Vulkan.Extensions.VK_KHR_surface
import qualified Vulkan.Extensions.VK_KHR_surface as SurfaceCapabilitiesKHR (SurfaceCapabilitiesKHR (..))
import qualified Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero
import Data.Bifunctor
import Control.Monad.IO.Class

data SwapchainInfo = SwapchainInfo
    { siSwapchain :: SwapchainKHR
    , siSwapchainReleaseKey :: ReleaseKey
    , siPresentMode :: PresentModeKHR
    , siSurfaceFormat :: SurfaceFormatKHR
    , siImageExtent :: Extent2D
    , siSurface :: SurfaceKHR
    }
    deriving (Generic)

data SwapchainResources = SwapchainResources
    { srInfo :: !SwapchainInfo
    , srImageViews :: Vector ImageView
    , srImages :: Vector Image
    , srRelease :: !RefCounted
    }
    deriving (Generic)

allocSwapchainResources ::
    (MonadResource m) =>
    DeviceParams ->
    -- | Previous swapchain, can be NULL_HANDLE
    SwapchainKHR ->
    GLFW.Window ->
    SurfaceKHR ->
    m SwapchainResources
allocSwapchainResources devParams@(DeviceParams{..}) oldSwapchain  window surface = do
    srInfo@SwapchainInfo{..} <- createSwapchain devParams oldSwapchain window surface
    (_, srImages) <- getSwapchainImagesKHR dpDevice siSwapchain

    (imageViewKeys, srImageViews) <- fmap V.unzip . V.forM srImages $ \image -> do
        let imageCreateInfo image =
                zero
                    { ImageViewCreateInfo.image = image
                    , viewType = IMAGE_VIEW_TYPE_2D
                    , ImageViewCreateInfo.format = SurfaceFormatKHR.format siSurfaceFormat
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
        withImageView dpDevice (imageCreateInfo image) Nothing allocate

    -- This refcount is released in 'recreateSwapchainResources'
    srRelease <- newRefCounted $ do
        traverse_ release imageViewKeys
        release siSwapchainReleaseKey

    return $ SwapchainResources{..}

recreateSwapchainResources ::
    (MonadResource m) =>
    GLFW.Window ->
    SwapchainResources ->
    DeviceParams ->
    m SwapchainResources
recreateSwapchainResources window oldResources devParams = do
    let oldSwapchain = siSwapchain . srInfo $ oldResources
        oldSurface = siSurface . srInfo $ oldResources
    r <- allocSwapchainResources devParams oldSwapchain window oldSurface
    releaseRefCounted (srRelease oldResources)
    return r

createSwapchain ::
    (MonadResource m) =>
    DeviceParams ->
    -- | old swapchain, can be NULL_HANDLE
    SwapchainKHR ->
    GLFW.Window ->
    SurfaceKHR ->
    m SwapchainInfo
createSwapchain devParams@(DeviceParams{..}) oldSwapchain window surface = do
    surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR dpPhysicalDevice surface
    (_, availableFormats) <- getPhysicalDeviceSurfaceFormatsKHR dpPhysicalDevice surface
    (_, availablePresentModes) <- getPhysicalDeviceSurfacePresentModesKHR dpPhysicalDevice surface

    surfaceFormat <- chooseSurfaceFormat availableFormats

    let desiredPresentModes = [PRESENT_MODE_MAILBOX_KHR]
    let requiredUsageFlags = [IMAGE_USAGE_COLOR_ATTACHMENT_BIT] :: [ImageUsageFlagBits]

    fbSize <- liftIO $ uncurry Extent2D . bimap fromIntegral fromIntegral <$> GLFW.getFramebufferSize window

    presentMode <- case filter (`V.elem` availablePresentModes) desiredPresentModes of
        [] -> return PRESENT_MODE_FIFO_KHR
        (x : _) -> return x

    let imageExtent = case currentExtent (surfaceCaps :: SurfaceCapabilitiesKHR) of
            Extent2D w h | w == maxBound, h == maxBound -> fbSize
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
                then (SHARING_MODE_EXCLUSIVE, [snd dpGraphicsQueue])
                else (SHARING_MODE_CONCURRENT, [snd dpGraphicsQueue, snd dpPresentQueue])

    let swapchainCreateInfo =
            SwapchainCreateInfoKHR
                { next = ()
                , flags = zero
                , surface = surface
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

    (releaseKey, swapchain) <- withSwapchainKHR dpDevice swapchainCreateInfo Nothing allocate

    return $ SwapchainInfo swapchain releaseKey presentMode surfaceFormat imageExtent surface

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
