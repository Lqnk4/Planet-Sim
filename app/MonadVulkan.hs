module MonadVulkan (
    GlobalHandles (..),
    RecycledResources (..),
    initGlobalHandles,
    sameGraphicsPresentQueues,
) where

import Control.Concurrent.Chan.Unagi
import Control.Monad.Trans.Resource
import Data.Word
import Vulkan.Core10
import Control.Monad.IO.Class

type QueueFamilyIndex = Word32

-- | A bunch of global, unchanging state we cart around
data GlobalHandles = GlobalHandles
    { ghInstance :: Instance
    , ghPhysicalDevice :: PhysicalDevice
    , ghDevice :: Device
    , ghGraphicsQueue :: (Queue, QueueFamilyIndex)
    , ghPresentQueue :: (Queue, QueueFamilyIndex)
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

sameGraphicsPresentQueues :: GlobalHandles -> Bool
sameGraphicsPresentQueues = (==) <$> (snd . ghGraphicsQueue) <*> (snd . ghPresentQueue)

data RecycledResources = RecycledResources
    { fImageAvailableSemaphore :: Semaphore
    , fRenderFinishedSemaphore :: Semaphore
    , fInFlightFence :: Fence
    , fCommandPool :: CommandPool
    }

initGlobalHandles ::
    (MonadResource m) =>
    Instance ->
    PhysicalDevice ->
    Device ->
    -- | Graphics Queue
    (Queue, QueueFamilyIndex) ->
    -- | Present Queue
    (Queue, QueueFamilyIndex) ->
    m GlobalHandles
initGlobalHandles ghInstance ghPhysicalDevice ghDevice ghGraphicsQueue ghPresentQueue = do
    (bin, nib) <- liftIO newChan
    let ghRecycleBin = writeChan bin
        ghRecycleNib = do
            (try, block) <- tryReadChan nib
            maybe (Left block) Right <$> tryRead try
    return GlobalHandles{..}
