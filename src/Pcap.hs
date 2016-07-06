module Pcap (readPcap)

where

import Data.Word

import Data.ByteString
import qualified Net.ARP as ARP
import qualified Net.Ethernet as Ethernet
import Net.Packet
import Net.PacketParsing
import Network.Pcap


readPcap :: FilePath -> IO (PktHdr, Maybe (Ethernet.Packet ARP.Packet))
readPcap file = do
    handle <- openOffline file
    (header, packet) <- nextBS handle
    let size = hdrCaptureLength header
    let inPacket = toInPack $ bsToArr size packet
    print . elems $ bsToArr size packet
    return (header, doParse inPacket)
  where
    bsToArr :: Word32 -> ByteString -> UArray Int Word8
    bsToArr size = listArray (0, fromIntegral size-1) . unpack
