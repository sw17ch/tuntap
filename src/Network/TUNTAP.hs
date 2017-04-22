{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls,
             CPP #-}
module Network.TUNTAP (
    start,
    finish,
    openTAP,
    closeTAP,
    bringUp,
    setMTU,
    setIP,
    setMask,
    setIPv6,

    getMAC,

    TAP,
    Type (..),
    Packet,
    DevMAC,

    readTAP,
    writeTAP,

    withTAP,
) where

import Control.Exception
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Data.Word
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as BS

data EthernetFrame

-- | A DevMAC is a hardware address 48 bits long.
type DevMAC = [Word8]

-- | Expects a single argument -- a list 6 Word8's long
mkDevMAC :: [Word8] -> [Word8]
mkDevMAC m = case length m of
                   6 -> m
                   _ -> error "A DevMAC is 6 bytes! No more! No less!"

-- A TAP device desciptor
data TAP = TAP (Ptr TAP)
    deriving (Show)

data Type = TAPType
          | TUNType

maxPktSize :: Int
maxPktSize = 1560

-- | Read/Written to the TUN/TAP device.
type Packet = BS.ByteString
    
-- |Allocate a TAP resource
start :: IO TAP
start = init_tap_ffi >>= (return . TAP)

-- |Deallocate a TAP resource
finish :: TAP -> IO CInt
finish (TAP p) = finish_tap_ffi p

-- |Open the TAP device
openTAP :: TAP -> String -> Type -> IO CInt
openTAP (TAP p) n TAPType = withCString n (\s -> open_tap_ffi p s 0)
openTAP (TAP p) n TUNType = withCString n (\s -> open_tap_ffi p s 1)

-- |Close the TAP device
closeTAP :: TAP -> IO CInt
closeTAP (TAP p) = close_tap_ffi p

-- |Bring up the TAP device
bringUp :: TAP -> IO CInt
bringUp (TAP p) = bring_up_tap_ffi p

-- |Set the MTU of the TAP device
setMTU :: TAP -> Int -> IO CInt
setMTU (TAP p) m = set_mtu_ffi p (fromIntegral m)

-- |Set the IPv4 address of the TAP device
setIP :: TAP -> Word32 -> IO CInt
setIP (TAP p) a = set_ip_ffi p (fromIntegral a)

-- |Set the network mask of the TAP device
setMask :: TAP -> Word32 -> IO CInt
setMask (TAP p) m = set_mask_ffi p (fromIntegral m)

-- |Set the IPv6 address of the TAP device
setIPv6 :: TAP -> (Word32, Word32, Word32, Word32) -> Word32 -> IO CInt
setIPv6 (TAP p) (a0, a1, a2, a3) pfxlen = set_ipv6_ffi p (fromIntegral a0) (fromIntegral a1) (fromIntegral a2) (fromIntegral a3) (fromIntegral pfxlen)

-- |Get the MAC address assigned to the TAP device
getMAC :: TAP -> IO DevMAC
getMAC (TAP p) = allocaArray 6 g
    where g m = do get_mac_ffi p m
                   peekArray 6 m >>= (return . mkDevMAC . (map fromIntegral))

-- |Read a packet from the TAP device
readTAP :: TAP -> IO Packet
readTAP (TAP t) = do
    pkt <- mallocForeignPtrBytes maxPktSize
    len <- go pkt
    return $ mk pkt len
    where 
          go pkt = do
            len <- mask_ $ withForeignPtr pkt $ \pkt' -> read_tap_ffi t pkt' mps
            case len of
                0 -> go pkt
                _ -> return len
          mk p l = let p' = castForeignPtr p
                       l' = fromIntegral l
                   in BI.fromForeignPtr p' 0 l'
          mps = fromIntegral maxPktSize

-- |Write a packet to the TAP device
writeTAP :: TAP -> Packet -> IO CInt
writeTAP (TAP t) p = withForeignPtr pkt $ \pkt' -> do
    wlen <- write_tap_ffi t pkt' (fromIntegral len)
    return wlen
    where (pkt,len) = let (p',_,l) = BI.toForeignPtr p
                      in (castForeignPtr p',l)

-- Some bracketing functions
    
-- |Accept an action and an MTU. Allocate a TAP and
-- pass it to the action. Clean up when finished with
-- the action.
withTAP :: String -> Int -> (TAP -> IO a) -> IO a
withTAP n m a =
  bracket start finish $ \tap ->
    bracket_ (openTAP tap n TAPType) (closeTAP tap) $
      setMTU tap m >> bringUp tap >> a tap

-- tap_desc_t * init_tap();
foreign import CALLCONV safe "help.h init_tap" init_tap_ffi :: IO (Ptr TAP)

-- void finish_tap(tap_desc_t * td);
foreign import CALLCONV safe "help.h finish_tap" finish_tap_ffi   :: (Ptr TAP) -> IO CInt

-- int32_t open_tap(tap_desc_t * td);
foreign import CALLCONV safe "help.h open_tap" open_tap_ffi :: (Ptr TAP) -> CString -> CInt -> IO CInt

-- int32_t close_tap(tap_desc_t * td);
foreign import CALLCONV safe "help.h close_tap" close_tap_ffi :: (Ptr TAP) -> IO CInt

-- int32_t bring_up_tap(tap_desc_t * td);
foreign import CALLCONV safe "help.h bring_up_tap" bring_up_tap_ffi :: (Ptr TAP) -> IO CInt

-- int32_t set_mtu(tap_desc_t * td, uint32_t mtu);
foreign import CALLCONV safe "help.h set_mtu" set_mtu_ffi :: (Ptr TAP) -> CUInt -> IO CInt

-- int32_t set_ip(tap_desc_t * td, uint32_t ip);
foreign import CALLCONV safe "help.h set_ip" set_ip_ffi :: (Ptr TAP) -> CUInt -> IO CInt

-- int32_t set_mask(tap_desc_t * td, uint32_t mask);
foreign import CALLCONV safe "help.h set_mask" set_mask_ffi :: (Ptr TAP) -> CUInt -> IO CInt

-- int32_t set_ipv6(tap_desc_t * td, uint32_t ip0, uint32_t ip1, uint32_t ip2, uint32_t ip3, uint32_t prefixlen);
foreign import CALLCONV safe "help.h set_ipv6" set_ipv6_ffi :: (Ptr TAP) -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO CInt

-- int32_t get_mac(tap_desc_t * td, DevMAC mac);
foreign import CALLCONV safe "help.h get_mac" get_mac_ffi :: (Ptr TAP) -> (Ptr CUChar) -> IO CInt

-- int32_t read_tap(tap_desc_t * td, int8_t * buf, int32_t len)
foreign import CALLCONV safe "help.h read_tap" read_tap_ffi :: (Ptr TAP) -> (Ptr EthernetFrame) -> CInt -> IO CInt

-- int32_t write_tap(tap_desc_t * td, const int8_t * buf, int32_t len)
foreign import CALLCONV safe "help.h write_tap" write_tap_ffi :: (Ptr TAP) -> (Ptr EthernetFrame) -> CInt -> IO CInt
