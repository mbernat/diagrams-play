{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Pcap
    ( PcapTime(..)
    , Peers(..)
    , arpProxy
    , ethernetProxy
    , ipv4Proxy
    , messageToGraphOps
    , readParse
    , readPcap
    , readTcpPeers
    , tcpProxy
    , udpProxy
    )
where

import Data.Monoid ((<>))
import Data.Proxy
import Data.Word

import Data.ByteString
import qualified Data.Text as Text
import qualified Net.ARP as ARP
import qualified Net.IPv4 as IPv4
import qualified Net.TCP as TCP
import qualified Net.UDP as UDP
import qualified Net.Ethernet as Ethernet
import Net.Packet
import Net.PacketParsing
import Net.PortNumber
import qualified Network.Pcap as Pcap

import GraphStream


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

newtype PcapTime = PcapTime { unPcapTime :: (Word32, Word32) }
  deriving (Eq, Show)

instance Ord PcapTime where
    (PcapTime (s1, u1)) <= (PcapTime (s2, u2)) =
        s1 <= s2 || (s1 == s2 && u1 <= u2)

data Message = Message
    { peers :: Peers
    , timestamp :: PcapTime
    }
  deriving (Show)

messageToGraphOps :: Message -> GraphStream PcapTime
messageToGraphOps Message{..} =
    [ (timestamp, AddNode from "")
    , (timestamp, AddNode to "")
    , (timestamp, AddEdge (from, to) "")
    ]
  where
    from = Text.pack $ show (source peers)-- <> show (sourcePort peers)
    to = Text.pack $ show (dest peers)-- <> show (destPort peers)

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

readTcpPeers :: FilePath -> IO [Message]
readTcpPeers file = do
    hps <- readParse tcpProxy file
    pure $ Prelude.concatMap mkMessage hps
  where
    mkMessage (_, Nothing) = []
    mkMessage (header, Just packet) = pure Message
        { peers = tcpToPeers packet
        , timestamp = PcapTime (Pcap.hdrSeconds header, Pcap.hdrUseconds header)
        }

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
