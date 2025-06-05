module Init (
    Init.createInstance,
    Init.createDevice,
    DeviceParams (..),
    dpGraphicsQueueIndex,
    dpPresentQueueIndex,
    sameGraphicsPresentQueues,
) where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.Bitraversable (bisequence)
import Data.Bits
import qualified Data.ByteString as BS
import Data.List (nub)
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
    { dpPhysicalDevice :: !PhysicalDevice
    , dpDevice :: !Device
    , dpGraphicsQueue :: !(Queue, Word32)
    , dpPresentQueue :: !(Queue, Word32)
    }
    deriving (Show, Eq)

dpGraphicsQueueIndex :: DeviceParams -> Word32
dpGraphicsQueueIndex = snd . dpGraphicsQueue

dpPresentQueueIndex :: DeviceParams -> Word32
dpPresentQueueIndex = snd . dpPresentQueue

sameGraphicsPresentQueues :: DeviceParams -> Bool
sameGraphicsPresentQueues = (==) <$> dpGraphicsQueueIndex <*> dpPresentQueueIndex

createDevice :: (MonadResource m, MonadThrow m) => Instance -> SurfaceKHR -> m DeviceParams
createDevice inst surf = do
    (pdi, phys) <-
        VkUtils.pickPhysicalDevice inst (physicalDeviceInfo surf) id >>= \case
            Nothing -> throw (AssertionFailed "Unable to find suitable physical device")
            Just x -> return x
    -- devName <- VkUtils.physicalDeviceName phys
    let graphicsQueueFamilyIndex = pdiGraphicsQueueFamilyIndex pdi
        presentQueueFamilyIndex = pdiPresentQueueFamilyIndex pdi
        uniqueQueueFamilyIndices = V.fromList $ nub [graphicsQueueFamilyIndex, presentQueueFamilyIndex]

        queueCreateInfos = V.map (\index -> SomeStruct zero{DeviceQueueCreateInfo.queueFamilyIndex = index, queuePriorities = [1]}) uniqueQueueFamilyIndices

        deviceCreateInfo =
            zero
                { queueCreateInfos = queueCreateInfos
                , DeviceCreateInfo.enabledExtensionNames = [KHR_SWAPCHAIN_EXTENSION_NAME]
                }
    (_, dev) <- withDevice phys deviceCreateInfo Nothing allocate
    graphicsQueue <- getDeviceQueue dev graphicsQueueFamilyIndex 0
    presentQueue <- getDeviceQueue dev presentQueueFamilyIndex 0

    return $ DeviceParams phys dev (graphicsQueue, graphicsQueueFamilyIndex) (presentQueue, presentQueueFamilyIndex)

deviceHasSwapchain :: (MonadIO m) => PhysicalDevice -> SurfaceKHR -> m Bool
deviceHasSwapchain dev surf = do
    (_, extensions) <- enumerateDeviceExtensionProperties dev Nothing
    let hasSwapchain = V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ==) . extensionName) extensions
    formats <- getPhysicalDeviceSurfaceFormatsKHR dev surf
    presentModes <- getPhysicalDeviceSurfacePresentModesKHR dev surf
    return $ hasSwapchain && not (null formats) && not (null presentModes)

data PhysicalDeviceInfo = PhysicalDeviceInfo
    { pdiDeviceType :: PhysicalDeviceType
    , pdiTotalMemory :: Word64
    , pdiGraphicsQueueFamilyIndex :: Word32
    , pdiPresentQueueFamilyIndex :: Word32
    }
    deriving (Eq)

instance Ord PhysicalDeviceInfo where
    compare di1 di2 =
        pdiDeviceType di1 `compareDeviceType` pdiDeviceType di2
            <> (sameGraphicsPresentQueues di1 `compare` sameGraphicsPresentQueues di2)
            <> (pdiTotalMemory di1 `compare` pdiTotalMemory di2)
      where
        compareDeviceType :: PhysicalDeviceType -> PhysicalDeviceType -> Ordering
        compareDeviceType t1 t2 = mapPriority t1 `compare` mapPriority t2
        mapPriority :: PhysicalDeviceType -> Int
        mapPriority = \case
            PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 5
            PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 4
            PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 3
            PHYSICAL_DEVICE_TYPE_CPU -> 2
            PHYSICAL_DEVICE_TYPE_OTHER -> 1
        sameGraphicsPresentQueues = (==) <$> pdiGraphicsQueueFamilyIndex <*> pdiPresentQueueFamilyIndex

-- | Requires the device to have a graphics and present queue
physicalDeviceInfo ::
    (MonadIO m) => SurfaceKHR -> PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo surf phys = runMaybeT $ do
    -- We must be able to use the swapchain extension
    guard =<< deviceHasSwapchain phys surf

    pdiDeviceType <- deviceType <$> getPhysicalDeviceProperties phys

    -- It must have a graphics and present queue
    (pdiGraphicsQueueFamilyIndex, pdiPresentQueueFamilyIndex) <- do
        queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties phys
        let isGraphicsQueue q = (QUEUE_GRAPHICS_BIT .&&. queueFlags q) && (queueCount q > 0)
        let isPresentQueue i = getPhysicalDeviceSurfaceSupportKHR phys i surf
            graphicsQueueIndices =
                fromIntegral . fst
                    <$> V.filter
                        (isGraphicsQueue . snd)
                        (V.indexed queueFamilyProperties)
        presentQueueIndices <-
            fmap (fromIntegral . fst)
                <$> V.filterM
                    (isPresentQueue . fromIntegral . fst)
                    (V.indexed queueFamilyProperties)
        MaybeT $ do
            return $
                dupe <$> V.find (`V.elem` presentQueueIndices) graphicsQueueIndices
                    <|> bisequence (graphicsQueueIndices V.!? 0, presentQueueIndices V.!? 0)

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

dupe :: a -> (a, a)
dupe x = (x, x)
