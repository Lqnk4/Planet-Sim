module Init (
    Init.createInstance,
    Init.createDevice,
    DeviceParams (..),
) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Data.Word
import Vulkan.CStruct.Extends
import Vulkan.Core10
import qualified Vulkan.Core10 as ApplicationInfo (ApplicationInfo (..))
import qualified Vulkan.Core10 as DeviceCreateInfo (DeviceCreateInfo (..))
import qualified Vulkan.Core10 as DeviceQueueCreateInfo (DeviceQueueCreateInfo (..))
import qualified Vulkan.Core10 as MemoryHeap (MemoryHeap (..))
import Vulkan.Core13
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Requirement
import qualified Vulkan.Utils.Initialization as VkUtils
import Vulkan.Zero

myApiVersion :: Word32
myApiVersion = API_VERSION_1_3

createInstance :: forall m. (MonadResource m) => [BS.ByteString] -> m Instance
createInstance extraExtensions = do
    VkUtils.createDebugInstanceFromRequirements
        [RequireInstanceExtension Nothing n minBound | n <- extraExtensions]
        []
        zero
            { applicationInfo =
                Just
                    zero
                        { applicationName = Nothing
                        , ApplicationInfo.apiVersion = myApiVersion
                        }
            }

data DeviceParams = DeviceParams
    { dpDeviceName :: !Text
    , dpPhysicalDevice :: !PhysicalDevice
    , dpDevice :: !Device
    , dpGraphicsQueue :: !Queue
    , dpGraphicsQueueFamilyIndex :: !Word32
    , dpPresentQueue :: !Queue
    , dpPresentQueueFamilyIndex :: !Word32
    }
    deriving (Show, Eq)

createDevice :: (MonadResource m, MonadThrow m) => Instance -> SurfaceKHR -> m DeviceParams
createDevice inst surf = do
    (pdi, phys) <-
        VkUtils.pickPhysicalDevice inst (physicalDeviceInfo surf) pdiTotalMemory >>= \case
            Nothing -> throw (AssertionFailed "Unable to find suitable physical device")
            Just x -> return x

    devName <- VkUtils.physicalDeviceName phys
    let graphicsQueueFamilyIndex = pdiGraphicsQueueFamilyIndex pdi
        presentQueueFamilyIndex = pdiPresentQueueFamilyIndex pdi
        deviceCreateInfo =
            zero
                { queueCreateInfos =
                    [ SomeStruct
                        zero
                            { DeviceQueueCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex
                            , queuePriorities = [1]
                            }
                    , SomeStruct
                        zero
                            { DeviceQueueCreateInfo.queueFamilyIndex = presentQueueFamilyIndex
                            , queuePriorities = [1]
                            }
                    ]
                , DeviceCreateInfo.enabledExtensionNames = [KHR_SWAPCHAIN_EXTENSION_NAME]
                }
    (_, dev) <- withDevice phys deviceCreateInfo Nothing allocate
    graphicsQueue <- getDeviceQueue dev graphicsQueueFamilyIndex 0
    presentQueue <- getDeviceQueue dev presentQueueFamilyIndex 0

    return $ DeviceParams devName phys dev graphicsQueue graphicsQueueFamilyIndex presentQueue presentQueueFamilyIndex

deviceHasSwapchain :: (MonadIO m) => PhysicalDevice -> SurfaceKHR -> m Bool
deviceHasSwapchain dev surf = do
    (_, extensions) <- enumerateDeviceExtensionProperties dev Nothing
    let hasSwapchain = V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ==) . extensionName) extensions
    formats <- getPhysicalDeviceSurfaceFormatsKHR dev surf
    presentModes <- getPhysicalDeviceSurfacePresentModesKHR dev surf
    return $ hasSwapchain && not (null formats) && not (null presentModes)

data PhysicalDeviceInfo = PhysicalDeviceInfo
    { pdiTotalMemory :: Word64
    , pdiGraphicsQueueFamilyIndex :: Word32
    , pdiPresentQueueFamilyIndex :: Word32
    }
    deriving (Eq)

instance Ord PhysicalDeviceInfo where
    compare di1 di2 = pdiTotalMemory di1 `compare` pdiTotalMemory di2

{- | Requires the device to have a graphics queue

The graphics queue index will be able to present to the specified surface
-}
physicalDeviceInfo ::
    (MonadIO m) => SurfaceKHR -> PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo surf phys = runMaybeT $ do
    -- We must be able to use the swapchain extension
    guard =<< deviceHasSwapchain phys surf

    -- It must have a graphics and present queue
    (pdiGraphicsQueueFamilyIndex, pdiPresentQueueFamilyIndex) <- do
        -- TODO: support different queues for graphics and present, but prioritize using the same for both
        queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties phys
        let isGraphicsQueue q =
                (QUEUE_GRAPHICS_BIT .&&. queueFlags q) && (queueCount q > 0)
            graphicsQueueIndices =
                fromIntegral . fst
                    <$> V.filter
                        (isGraphicsQueue . snd)
                        (V.indexed queueFamilyProperties)
        let isPresentQueue i = getPhysicalDeviceSurfaceSupportKHR phys i surf
        presentQueueIndices <- V.filterM isPresentQueue graphicsQueueIndices
        MaybeT (pure $ presentQueueIndices V.!? 0)

    -- Score based on the total memory
    pdiTotalMemory <- do
        heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
        pure $ sum (MemoryHeap.size <$> heaps)

    return PhysicalDeviceInfo{..}

--
-- Utils
--

infixl 4 .&&.
(.&&.) :: (Bits a) => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
