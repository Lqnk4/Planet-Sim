{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Init
    ( Init.createInstance
    , Init.createDevice
    , DeviceParams(..)
    ) where

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
import qualified Vulkan.Core10 as MemoryHeap (MemoryHeap (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10 as Vk.ApplicationInfo (ApplicationInfo (..))
import qualified Vulkan.Core10 as Vk.DeviceCreateInfo (DeviceCreateInfo (..))
import qualified Vulkan.Core10 as Vk.DeviceQueueCreateInfo (DeviceQueueCreateInfo (..))
import qualified Vulkan.Core13 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk
import qualified Vulkan.Requirement as Vk
import qualified Vulkan.Utils.Initialization as VkUtils
import qualified Vulkan.Zero as Vk

myApiVersion :: Word32
myApiVersion = Vk.API_VERSION_1_3

createInstance :: forall m. (MonadResource m) => [BS.ByteString] -> m Vk.Instance
createInstance extraExtensions = do
    VkUtils.createDebugInstanceFromRequirements
        [Vk.RequireInstanceExtension Nothing n minBound | n <- extraExtensions]
        []
        Vk.zero
            { Vk.applicationInfo =
                Just
                    Vk.zero
                        { Vk.applicationName = Nothing
                        , Vk.ApplicationInfo.apiVersion = myApiVersion
                        }
            }

data DeviceParams = DeviceParams
    { dpDeviceName :: Text
    , dpPhysicalDevice :: Vk.PhysicalDevice
    , dpDevice :: Vk.Device
    , dpGraphicsQueue :: Vk.Queue
    -- ^ Also the present queue
    , dpGraphicsQueueFamilyIndex :: Word32
    }
    deriving (Show)

createDevice :: (MonadResource m, MonadThrow m) => Vk.Instance -> Vk.SurfaceKHR -> m DeviceParams
createDevice inst surf = do
    (pdi, phys) <-
        pickPhysicalDevice inst (physicalDeviceInfo surf) >>= \case
            Nothing -> error "Unable to find suitable physical device"
            Just x -> return x
    devName <- physicalDeviceName phys

    let graphicsQueueFamilyIndex = pdiGraphicsQueueFamilyIndex pdi
        deviceCreateInfo =
            Vk.zero
                { Vk.queueCreateInfos =
                    [ SomeStruct
                        Vk.zero
                            { Vk.DeviceQueueCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex
                            , Vk.queuePriorities = [1]
                            }
                    ]
                , Vk.DeviceCreateInfo.enabledExtensionNames = [Vk.KHR_SWAPCHAIN_EXTENSION_NAME]
                }
    (_, dev) <- Vk.withDevice phys deviceCreateInfo Nothing allocate
    graphicsQueue <- Vk.getDeviceQueue dev graphicsQueueFamilyIndex 0

    return $ DeviceParams devName phys dev graphicsQueue graphicsQueueFamilyIndex

pickPhysicalDevice ::
    (MonadIO m, MonadThrow m, Ord a) =>
    Vk.Instance ->
    (Vk.PhysicalDevice -> m (Maybe a)) ->
    m (Maybe (a, Vk.PhysicalDevice))
pickPhysicalDevice inst devscore = do
    (_, devs) <- Vk.enumeratePhysicalDevices inst
    scores <- catMaybes <$> sequence [ fmap (, d) <$> devscore d | d <- V.toList devs]
    return $ if null scores then Nothing else Just $ V.maximumBy (comparing fst) (V.fromList scores)

deviceHasSwapchain :: (MonadIO m) => Vk.PhysicalDevice -> m Bool
deviceHasSwapchain dev = do
    (_, extensions) <- Vk.enumerateDeviceExtensionProperties dev Nothing
    pure $ V.any ((Vk.KHR_SWAPCHAIN_EXTENSION_NAME ==) . Vk.extensionName) extensions

physicalDeviceName :: (MonadIO m) => Vk.PhysicalDevice -> m Text
physicalDeviceName phys = do
    props <- Vk.getPhysicalDeviceProperties phys
    pure $ decodeUtf8 (Vk.deviceName props)

data PhysicalDeviceInfo = PhysicalDeviceInfo
    { pdiTotalMemory :: Word64
    , pdiGraphicsQueueFamilyIndex :: Word32
    }
    deriving (Eq, Ord)

{- | Requires the device to have a graphics queue

The graphics queue index will be able to present to the specified surface
-}
physicalDeviceInfo ::
    (MonadIO m) => Vk.SurfaceKHR -> Vk.PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo surf phys = runMaybeT $ do
    -- We must be able to use the swapchain extension
    guard =<< deviceHasSwapchain phys

    -- It must have a graphics and present queue
    pdiGraphicsQueueFamilyIndex <- do
        queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties phys
        let isGraphicsQueue q =
                (Vk.QUEUE_GRAPHICS_BIT .&&. Vk.queueFlags q) && (Vk.queueCount q > 0)
            graphicsQueueIndices =
                fromIntegral . fst
                    <$> V.filter
                        (isGraphicsQueue . snd)
                        (V.indexed queueFamilyProperties)
        let isPresentQueue i = Vk.getPhysicalDeviceSurfaceSupportKHR phys i surf
        presentQueueIndices <- V.filterM isPresentQueue graphicsQueueIndices
        MaybeT (pure $ presentQueueIndices V.!? 0)

    -- Score based on the total memory
    pdiTotalMemory <- do
        heaps <- Vk.memoryHeaps <$> Vk.getPhysicalDeviceMemoryProperties phys
        pure $ sum (MemoryHeap.size <$> heaps)

    return PhysicalDeviceInfo{..}

--
-- Utils
--

infixl 4 .&&.
(.&&.) :: (Bits a) => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
