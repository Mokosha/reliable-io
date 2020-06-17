{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reliable.IO ( 
    initialize
  , terminate

  , LogLevel(..)
  , logLevel

  , EndpointConfig
  , defaultConfig

  , TransmitPacketFunction
  , ProcessPacketFunction
  
  , Endpoint
  , createEndpoint
  , destroyEndpoint
) where 

-------------------------------------------------------------------------------

import Bindings.Reliable.IO

import Control.Monad (unless)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Ptr (Ptr)
import GHC.Word (Word8, Word16)
import Foreign.C.Types (CDouble(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (freeHaskellFunPtr)
import Foreign.Storable (poke, Storable(..))

-------------------------------------------------------------------------------

-- | Initializes the @reliable.io@ library runtime. This should be called before
-- any additional functions in this library. Throws an
-- t'Control.Exception.IOException' on failure.
initialize :: IO ()
initialize = do
    result <- c'reliable_init
    unless (result == c'RELIABLE_OK) $
      fail $ "Failed to initialize reliable.io. Result: " <> show result
    return ()

-- | Terminates the @reliable.io@ library runtime. This should be called only
-- after all other library functions terminate.
terminate :: IO ()
terminate = c'reliable_term

-- | Specifies the logging behavior of @reliable.io@. Note, this logging behavior
-- is called from C calls to @printf@ and therefore might interfere with the
-- Haskell runtime (such as 'putStrLn').
data LogLevel = LogLevel'None
              | LogLevel'Info
              | LogLevel'Error
              | LogLevel'Debug
    deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, Data, Typeable)

-- | Set the @reliable.io@ 'LogLevel'. The default is 'LogLevel'None'.
logLevel :: LogLevel -> IO ()
logLevel LogLevel'None  = c'reliable_log_level c'RELIABLE_LOG_LEVEL_NONE
logLevel LogLevel'Info  = c'reliable_log_level c'RELIABLE_LOG_LEVEL_INFO
logLevel LogLevel'Error = c'reliable_log_level c'RELIABLE_LOG_LEVEL_ERROR
logLevel LogLevel'Debug = c'reliable_log_level c'RELIABLE_LOG_LEVEL_DEBUG

newtype EndpointConfig = EndpointConfig { 
    generateConfig :: Ptr C'reliable_config_t -> IO ()
    }

defaultConfig :: EndpointConfig
defaultConfig = EndpointConfig c'reliable_default_config

data EndpointCallbacks = EndpointCallbacks
    { _endpointCallbacksXmit :: C'transmit_packet_function_t
    , _endpointCallbacksRecv :: C'process_packet_function_t
    }

data Endpoint = Endpoint
    { _endpointPtr :: Ptr C'reliable_endpoint_t
    , _endpointCallbacks :: EndpointCallbacks
    }

type TransmitPacketFunction = Int -> Word16 -> Ptr Word8 -> Int -> IO ()

mkTransmitPacketFunction :: TransmitPacketFunction -> IO C'transmit_packet_function_t
mkTransmitPacketFunction fn = mk'transmit_packet_function_t $
    \_ a b c d -> fn (fromIntegral a) b c (fromIntegral d)

type ProcessPacketFunction = Int -> Word16 -> Ptr Word8 -> Int -> IO Int

mkProcessPacketFunction :: ProcessPacketFunction -> IO C'process_packet_function_t
mkProcessPacketFunction fn = mk'process_packet_function_t $
    \_ a b c d -> fromIntegral <$> fn (fromIntegral a) b c (fromIntegral d)

createEndpoint :: EndpointConfig
               -> Double
               -> TransmitPacketFunction
               -> ProcessPacketFunction
               -> IO Endpoint
createEndpoint cfg t xmitFn recvFn = alloca $ \ptr -> do
    generateConfig cfg ptr
    config <- peek ptr
    xmitCFn <- mkTransmitPacketFunction xmitFn
    recvCFn <- mkProcessPacketFunction recvFn
    poke ptr $ config {
        c'reliable_config_t'transmit_packet_function = xmitCFn,
        c'reliable_config_t'process_packet_function = recvCFn
    }
    endpoint <- c'reliable_endpoint_create ptr (CDouble t)
    return $ Endpoint endpoint (EndpointCallbacks xmitCFn recvCFn)

destroyEndpoint :: Endpoint -> IO ()
destroyEndpoint (Endpoint ptr cbs) = do
    c'reliable_endpoint_destroy ptr
    freeHaskellFunPtr $ _endpointCallbacksXmit cbs
    freeHaskellFunPtr $ _endpointCallbacksRecv cbs
    return ()