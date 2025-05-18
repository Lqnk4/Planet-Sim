{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadVulkan where

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import qualified Data.Vector as V
import Data.Word
import HasVulkan
import UnliftIO
import qualified Vulkan.Core10 as Vk
import qualified VulkanMemoryAllocator as VMA

newtype V a = V {unV :: ReaderT GlobalHandles (ResourceT IO) a}
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadResource)

instance MonadUnliftIO V where
    withRunInIO a = V $ withRunInIO (\r -> a (r . unV))

instance HasVulkan V where
  getInstance       = V (asks ghInstance)
  getGraphicsQueue  = V (asks ghGraphicsQueue)
  getPhysicalDevice = V (asks ghPhysicalDevice)
  getDevice         = V (asks ghDevice)
  getAllocator      = V (asks ghAllocator)

newtype CmdT m a = CmdT {unCmdT :: ReaderT Vk.CommandBuffer m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadFail
        , MonadIO
        , MonadResource
        , HasVulkan
        )

instance (MonadUnliftIO m) => MonadUnliftIO (CmdT m) where
    withRunInIO a = CmdT $ withRunInIO (\r -> a (r . unCmdT))

data GlobalHandles = GlobalHandles
    { ghInstance :: Vk.Instance
    , ghPhysicalDevice :: Vk.PhysicalDevice
    , ghDevice :: Vk.Device
    , ghAllocator :: VMA.Allocator
    , ghGraphicsQueue :: Vk.Queue
    , ghGraphcisQueueFamilyIndex :: Word32
    , ghCommandPools :: V.Vector Vk.CommandPool
    }
