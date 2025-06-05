module MonadVulkan(
    GlobalHandles(..),
    RecycledResources(..),
) where

import Data.Word
import Vulkan.Core10
import VulkanMemoryAllocator

-- | A bunch of global, unchanging state we cart around
data GlobalHandles = GlobalHandles
    { ghInstance :: Instance
    , ghPhysicalDevice :: PhysicalDevice
    , ghDevice :: Device
    , ghGraphicsQueue :: (Queue, Word32)
    , ghPresentQueue :: (Queue, Word32)
    , ghAllocator :: Allocator
    , ghRecycleBin :: RecycledResources -> IO ()
    -- ^ Filled with resources which aren't destroyed after finishing a frame,
    -- but instead are used by another frame which executes after that one is
    -- retired, (taken from ghRecycleNib)
    --
    -- Make sure not to pass any resources which were created with a frame-only
    -- scope however!
    , ghRecycleNib :: IO (Either (IO RecycledResources) RecycledResources)
    -- ^ The resources of prior frames waiting to be taken
    }

data RecycledResources = RecycledResources
    { fImageAvailableSemaphore :: Semaphore
    , fRenderFinishedSemaphore :: Semaphore
    , fInFlightFence :: Fence
    , fCommandPool :: CommandPool
    }
