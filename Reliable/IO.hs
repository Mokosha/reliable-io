{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reliable.IO ( 
    initialize
  , terminate

  , LogLevel(..)
  , logLevel

  , EndpointConfig
  , defaultConfig
  , setName
  , setMaxPacketSize
  , setPacketFragmentationLimit
  , setPacketFragmentSize
  , setMaxNumFragments
  , setAckBufferSize
  , setSentPacketsBufferSize
  , setReceivedPacketsBufferSize
  , setFragmentReassemblyBufferSize
  , setRTTSmoothingFactor
  , setPacketLossSmoothingFactor
  , setBandwidthSmoothingFactor
  , PacketType(..)
  , setPacketType

  , TransmitPacketFunction
  , ProcessPacketFunction
  
  , Endpoint
  , createEndpoint
  , destroyEndpoint
  , withEndpoint
  , nextPacketSequence
  , sendPacket
  , receivePacket
  , getAcks
  , clearAcks
  , reset
  , update
  , roundTripTime
  , packetLoss

  , BandwidthMeasurements(..)
  , bandwidth

  , Counter(..)
  , getCounter

) where 

-------------------------------------------------------------------------------

import Control.Monad         (unless)
import Data.Data             (Data)
import Data.Typeable         (Typeable)
import Data.Word             (Word8, Word16, Word64)
import GHC.Generics          (Generic)
import GHC.Ptr               (Ptr)
import Foreign.C.String      (withCStringLen)
import Foreign.C.Types       (CInt, CDouble(..), CFloat(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr           (freeHaskellFunPtr)
import Foreign.Storable      (poke, Storable(..))

import Bindings.Reliable.IO

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

setName :: String -> EndpointConfig -> EndpointConfig
setName s (EndpointConfig fn) = EndpointConfig $ \cfgPtr -> do
    fn cfgPtr
    withCStringLen s $ \(csPtr, csLen) -> do
        config <- peek cfgPtr
        cs <- peekArray csLen csPtr
        poke cfgPtr $ config { c'reliable_config_t'name = cs }

setConfig :: (C'reliable_config_t -> C'reliable_config_t)
          -> EndpointConfig -> EndpointConfig
setConfig fn (EndpointConfig mkCfg) = EndpointConfig $ \cfgPtr ->
    mkCfg cfgPtr >> peek cfgPtr >>= poke cfgPtr . fn

setConfigIntVal :: (CInt -> C'reliable_config_t -> C'reliable_config_t)
                -> Int -> EndpointConfig -> EndpointConfig
setConfigIntVal fn x = setConfig $ fn (fromIntegral x)

setConfigFloatVal :: (CFloat -> C'reliable_config_t -> C'reliable_config_t)
                  -> Float -> EndpointConfig -> EndpointConfig
setConfigFloatVal fn x = setConfig $ fn (CFloat x)

setMaxPacketSize :: Int -> EndpointConfig -> EndpointConfig
setMaxPacketSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'max_packet_size = sz }

setPacketFragmentationLimit :: Int -> EndpointConfig -> EndpointConfig
setPacketFragmentationLimit = setConfigIntVal $ \l config ->
    config { c'reliable_config_t'fragment_above = l }

setPacketFragmentSize :: Int -> EndpointConfig -> EndpointConfig
setPacketFragmentSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'fragment_size = sz }

setMaxNumFragments :: Int -> EndpointConfig -> EndpointConfig
setMaxNumFragments = setConfigIntVal $ \n config ->
    config { c'reliable_config_t'max_fragments = n }

setAckBufferSize :: Int -> EndpointConfig -> EndpointConfig
setAckBufferSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'ack_buffer_size = sz }

setSentPacketsBufferSize :: Int -> EndpointConfig -> EndpointConfig
setSentPacketsBufferSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'sent_packets_buffer_size = sz }

setReceivedPacketsBufferSize :: Int -> EndpointConfig -> EndpointConfig
setReceivedPacketsBufferSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'received_packets_buffer_size = sz }

setFragmentReassemblyBufferSize :: Int -> EndpointConfig -> EndpointConfig
setFragmentReassemblyBufferSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'fragment_reassembly_buffer_size = sz }

setRTTSmoothingFactor :: Float -> EndpointConfig -> EndpointConfig
setRTTSmoothingFactor = setConfigFloatVal $ \factor config ->
    config { c'reliable_config_t'rtt_smoothing_factor = factor }

setPacketLossSmoothingFactor :: Float -> EndpointConfig -> EndpointConfig
setPacketLossSmoothingFactor = setConfigFloatVal $ \factor config ->
    config { c'reliable_config_t'packet_loss_smoothing_factor = factor }

setBandwidthSmoothingFactor :: Float -> EndpointConfig -> EndpointConfig
setBandwidthSmoothingFactor = setConfigFloatVal $ \factor config ->
    config { c'reliable_config_t'bandwidth_smoothing_factor = factor }

data PacketType = PacketType'IPV4 | PacketType'IPV6
    deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, Data, Typeable)

setPacketType :: PacketType -> EndpointConfig -> EndpointConfig
setPacketType PacketType'IPV4 = setConfig $ \config ->
    config { c'reliable_config_t'packet_header_size = 28 }
setPacketType PacketType'IPV6 = setConfig $ \config ->
    config { c'reliable_config_t'packet_header_size = 48 }
    
data EndpointCallbacks = EndpointCallbacks
    { _endpointCallbacksXmit :: C'transmit_packet_function_t
    , _endpointCallbacksRecv :: C'process_packet_function_t
    }

data Endpoint = Endpoint
    { _endpointPtr :: Ptr C'reliable_endpoint_t
    , _endpointCallbacks :: EndpointCallbacks
    }

type TransmitPacketFunction = Word16 -> Ptr Word8 -> Int -> IO ()

mkTransmitPacketFunction :: TransmitPacketFunction -> IO C'transmit_packet_function_t
mkTransmitPacketFunction fn = mk'transmit_packet_function_t $
    \_ _ seqNo ptr -> fn seqNo ptr . fromIntegral

type ProcessPacketFunction = Word16 -> Ptr Word8 -> Int -> IO Int

mkProcessPacketFunction :: ProcessPacketFunction -> IO C'process_packet_function_t
mkProcessPacketFunction fn = mk'process_packet_function_t $
    \_ _ seqNo ptr -> fmap fromIntegral . fn seqNo ptr . fromIntegral

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

withEndpoint :: EndpointConfig
             -> Double
             -> TransmitPacketFunction
             -> ProcessPacketFunction
             -> (Endpoint -> IO a)
             -> IO a
withEndpoint cfg t xmit recv fn = do
    endpoint <- createEndpoint cfg t xmit recv
    result <- fn endpoint
    destroyEndpoint endpoint
    return result

nextPacketSequence :: Endpoint -> IO Word16
nextPacketSequence (Endpoint ptr _) = c'reliable_endpoint_next_packet_sequence ptr

sendPacket :: Endpoint -> Ptr Word8 -> Int -> IO ()
sendPacket (Endpoint epPtr _) pktPtr =
    c'reliable_endpoint_send_packet epPtr pktPtr . fromIntegral

receivePacket :: Endpoint -> Ptr Word8 -> Int -> IO ()
receivePacket (Endpoint epPtr _) pktPtr =
    c'reliable_endpoint_receive_packet epPtr pktPtr . fromIntegral

getAcks :: Endpoint -> IO [Word16]
getAcks (Endpoint epPtr _) = alloca $ \numAcksPtr -> do
    acksPtr <- c'reliable_endpoint_get_acks epPtr numAcksPtr
    numAcks <- peek acksPtr
    peekArray (fromIntegral numAcks) acksPtr

clearAcks :: Endpoint -> IO ()
clearAcks (Endpoint ptr _) = c'reliable_endpoint_clear_acks ptr

reset :: Endpoint -> IO ()
reset (Endpoint ptr _) = c'reliable_endpoint_reset ptr

update :: Endpoint -> Double -> IO ()
update (Endpoint ptr _) = c'reliable_endpoint_update ptr . CDouble

roundTripTime :: Endpoint -> IO Float
roundTripTime (Endpoint ptr _) = do
    (CFloat f) <- c'reliable_endpoint_rtt ptr
    return f

packetLoss :: Endpoint -> IO Float
packetLoss (Endpoint ptr _) = do
    (CFloat f) <- c'reliable_endpoint_packet_loss ptr
    return f

data BandwidthMeasurements = BandwidthMeasurements
    { bandwidthSentKbps :: Float
    , bandwidthReceivedKbps :: Float
    , bandwidthAckedKbps :: Float
    } deriving (Show, Read, Generic)

bandwidth :: Endpoint -> IO BandwidthMeasurements
bandwidth (Endpoint ptr _) =
    alloca $ \sentPtr ->
    alloca $ \receivedPtr ->
    alloca $ \ackedPtr -> do
        c'reliable_endpoint_bandwidth ptr sentPtr receivedPtr ackedPtr
        (CFloat sent) <- peek sentPtr
        (CFloat received) <- peek receivedPtr
        (CFloat acked) <- peek ackedPtr
        return $ BandwidthMeasurements sent received acked
    
data Counter
    = Counter'NumPacketsSent
    | Counter'NumPacketsReceived
    | Counter'NumPacketsAcked
    | Counter'NumPacketsStale
    | Counter'NumPacketsInvalid
    | Counter'NumPacketsTooLargeToSend
    | Counter'NumPacketsTooLargeToReceive
    | Counter'NumFragmentsSent
    | Counter'NumFragmentsReceived
    | Counter'NumFragmentsInvalid
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Data, Typeable)

getCounter :: Endpoint -> Counter -> IO Word64
getCounter (Endpoint ptr _) ctr =
    let ctrIdx = case ctr of
            Counter'NumPacketsSent              -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_SENT
            Counter'NumPacketsReceived          -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_RECEIVED
            Counter'NumPacketsAcked             -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_ACKED
            Counter'NumPacketsStale             -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_STALE
            Counter'NumPacketsInvalid           -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_INVALID
            Counter'NumPacketsTooLargeToSend    -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_TOO_LARGE_TO_SEND
            Counter'NumPacketsTooLargeToReceive -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_TOO_LARGE_TO_RECEIVE
            Counter'NumFragmentsSent            -> c'RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_SENT
            Counter'NumFragmentsReceived        -> c'RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_RECEIVED
            Counter'NumFragmentsInvalid         -> c'RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_INVALID
     in c'reliable_endpoint_counters ptr >>= flip peekElemOff ctrIdx