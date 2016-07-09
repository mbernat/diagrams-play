module Pcap
    ( Peers(..)
    , arpProxy
    , ethernetProxy
    , ipv4Proxy
    , readParse
    , readPcap
    , readTcpPeers
    , tcpProxy
    , udpProxy
    )
where

import Data.Proxy
import Data.Word

import Data.ByteString
import qualified Net.ARP as ARP
import qualified Net.IPv4 as IPv4
import qualified Net.TCP as TCP
import qualified Net.UDP as UDP
import qualified Net.Ethernet as Ethernet
import Net.Packet
import Net.PacketParsing
import Net.PortNumber
import qualified Network.Pcap as Pcap


data OpaqueContent = OpaqueContent
  deriving (Show)

instance Parse OpaqueContent where
    parse = pure OpaqueContent

data Peers = Peers
    { source :: IPv4.Addr
    , sourcePort :: Port
    , dest :: IPv4.Addr
    , destPort :: Port
    }
  deriving (Show)

type TCP a = Ethernet.Packet (IPv4.Packet (TCP.Packet a))
type UDP a = Ethernet.Packet (IPv4.Packet (UDP.Packet a))

tcpToPeers :: TCP OpaqueContent -> Peers
tcpToPeers eth = Peers
    { source = IPv4.source ipv4
    , sourcePort = TCP.sourcePort tcp
    , dest = IPv4.dest ipv4
    , destPort = TCP.destPort tcp
    }
  where
    ipv4 = Ethernet.content eth
    tcp = IPv4.content ipv4

readTcpPeers :: FilePath -> IO [Maybe Peers]
readTcpPeers file = do
    hps <- readParse tcpProxy file
    pure . flip Prelude.map hps $ \(_, packet) -> fmap tcpToPeers packet

ethernetProxy :: Proxy (Ethernet.Packet OpaqueContent)
ethernetProxy = Proxy

arpProxy :: Proxy (Ethernet.Packet ARP.Packet)
arpProxy = Proxy

ipv4Proxy :: Proxy (Ethernet.Packet (IPv4.Packet OpaqueContent))
ipv4Proxy = Proxy

tcpProxy :: Proxy (TCP OpaqueContent)
tcpProxy = Proxy

udpProxy :: Proxy (UDP OpaqueContent)
udpProxy = Proxy

readParse :: Parse a => Proxy a -> FilePath -> IO [(Pcap.PktHdr, Maybe a)]
readParse _packet file = do
    hps <- readPcap file
    pure $ Prelude.map parse' hps
  where
    parse' (header, packet) = (header, doParse packet)

readPcap :: FilePath -> IO [(Pcap.PktHdr, InPacket)]
readPcap file = do
    handle <- Pcap.openOffline file
    loop handle []
  where
    loop handle acc = do
        hp <- nextPacket handle
        if finished hp then pure acc else loop handle (hp:acc)

    finished (header, _) = Pcap.hdrSeconds header == 0

nextPacket :: Pcap.PcapHandle -> IO (Pcap.PktHdr, InPacket)
nextPacket handle = do
    (header, packet) <- Pcap.nextBS handle
    let size = Pcap.hdrCaptureLength header
    let inPacket = toInPack $ bsToArr size packet
    pure (header, inPacket)
  where
    bsToArr :: Word32 -> ByteString -> UArray Int Word8
    bsToArr size = listArray (0, fromIntegral size-1) . unpack
