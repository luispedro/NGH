module Data.NGH.Formats.FastQ
    ( fastQConduit
    , fastQparse
    , fastQread
    , fastQwrite
    ) where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.NGH.FastQ
import Data.Conduit

fastQConduit :: (Monad m) => (Word8 -> Word8) -> Conduit B.ByteString m DNAwQuality
fastQConduit qualN = start
    where
        start = NeedInput push0 close
        push0 h = NeedInput (push1 h) close
        push1 h sq = NeedInput (push2 h sq) close
        push2 h sq _ = NeedInput (push3 h sq) close
        push3 h sq qs = HaveOutput start (return ())
                            DNAwQuality { dna_seq=sq, header=h, qualities=B.map qualN qs }
        close = Done ()

fastQparse :: (Word8 -> Word8) -> [B.ByteString] -> [DNAwQuality]
fastQparse _ [] = []
fastQparse qualN (h:sq:_:qs:rest) = (first:fastQparse qualN rest)
    where first = DNAwQuality { dna_seq=sq, header=h, qualities=B.map qualN qs }
fastQparse _ _ = error "Data.NGH.FastQ.fastQparse: incomplete record"

fastQread :: (Word8 -> Word8) -> L.ByteString -> [DNAwQuality]
fastQread qualN = fastQparse qualN . map (S.concat . L.toChunks) . L8.lines

fastQwrite :: DNAwQuality -> L.ByteString
fastQwrite s = L.fromChunks
                    [header s
                    ,S8.pack "\n"
                    ,dna_seq s
                    ,S8.pack "\n+\n"
                    ,encodeQ $ qualities s
                    ,S8.pack "\n"]

encodeQ = S.map (+ (fromIntegral (64::Int)))
