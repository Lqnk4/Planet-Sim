module Init (
    createInstance,
    createDevice,
    DeviceParams (..),
) where

import Control.Applicative ((<|>))
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
import UnliftIO.Exception
import Vulkan.CStruct.Extends
import Vulkan.Core10 hiding (createDevice, createInstance)
import qualified Vulkan.Core10 as ApplicationInfo (ApplicationInfo (..))
import qualified Vulkan.Core10 as DeviceQueueCreateInfo (DeviceQueueCreateInfo (..))
import qualified Vulkan.Core10 as MemoryHeap (MemoryHeap (..))
import Vulkan.Core12
import Vulkan.Core13
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_timeline_semaphore
import Vulkan.Requirement
import qualified Vulkan.Utils.Initialization as VkUtils
import qualified Vulkan.Utils.Requirements.TH as U
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

createDevice :: (MonadResource m, MonadThrow m) => Instance -> SurfaceKHR -> m DeviceParams
createDevice inst surf = do
    (pdi, phys) <-
        VkUtils.pickPhysicalDevice inst (physicalDeviceInfo surf) id >>= \case
            Nothing -> throwString "Unable to find suitable physical device"
            Just x -> return x
    let graphicsQueueFamilyIndex = pdiGraphicsQueueFamilyIndex pdi
        presentQueueFamilyIndex = pdiPresentQueueFamilyIndex pdi
        uniqueQueueFamilyIndices = V.fromList $ nub [graphicsQueueFamilyIndex, presentQueueFamilyIndex]
        queueCreateInfos = V.map (\index -> SomeStruct zero{DeviceQueueCreateInfo.queueFamilyIndex = index, queuePriorities = [1]}) uniqueQueueFamilyIndices
        deviceCreateInfo =
            zero{queueCreateInfos = queueCreateInfos}
        requiredReqs = [U.reqs|
            1.2
            VK_KHR_swapchain
            VK_KHR_timeline_semaphore
            PhysicalDeviceTimelineSemaphoreFeatures.timelineSemaphore
        |]
        optionalReqs = [U.reqs||]

    dev <- VkUtils.createDeviceFromRequirements requiredReqs optionalReqs phys deviceCreateInfo
    graphicsQueue <- getDeviceQueue dev graphicsQueueFamilyIndex 0
    presentQueue <- getDeviceQueue dev presentQueueFamilyIndex 0

    return $ DeviceParams phys dev (graphicsQueue, graphicsQueueFamilyIndex) (presentQueue, presentQueueFamilyIndex)

deviceHasSwapchain :: (MonadIO m) => PhysicalDevice -> m Bool
deviceHasSwapchain dev = do
    (_, extensions) <- enumerateDeviceExtensionProperties dev Nothing
    return $ V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ==) . extensionName) extensions

deviceHasTimelineSemaphores :: (MonadIO m) => PhysicalDevice -> m Bool
deviceHasTimelineSemaphores phys = do
    let hasExt = do
            (_, extensions) <- enumerateDeviceExtensionProperties phys Nothing
            return $ V.any ((KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME ==) . extensionName) extensions
        hasFeat = do
            feats <- getPhysicalDeviceFeatures2KHR phys
            let _ ::& (PhysicalDeviceTimelineSemaphoreFeatures hasTimelineSemaphores :& ()) = feats
            return hasTimelineSemaphores
    hasExt <&&> hasFeat

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
    guard =<< deviceHasSwapchain phys
    guard =<< deviceHasTimelineSemaphores phys

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

(<&&>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
