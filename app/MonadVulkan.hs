{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module MonadVulkan where

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import qualified Data.Vector as V
import Data.Word
import qualified Vulkan.Core10 as Vk
import UnliftIO
import qualified VulkanMemoryAllocator as VMA
import HasVulkan

newtype V a = V {unV :: ReaderT GlobalHandles (ResourceT IO) a}
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadResource)

instance (MonadUnliftIO m) => MonadUnliftIO (CmdT m) where
    withRunInIO a = CmdT $ withRunInIO (\r -> a (r . unCmdT))

instance MonadUnliftIO V where
    withRunInIO a = V $ withRunInIO (\r -> a (r . unV))

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

data GlobalHandles = GlobalHandles
    { ghInstance :: Vk.Instance
    , ghPhysicalDevice :: Vk.PhysicalDevice
    , ghDevice :: Vk.Device
    , ghAllocator :: VMA.Allocator
    , ghGraphicsQueue :: Vk.Queue
    , ghGraphcisQueueFamilyIndex :: Word32
    , ghCommandPools :: V.Vector Vk.CommandPool
    }
