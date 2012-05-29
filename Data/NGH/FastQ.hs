module Data.NGH.FastQ
    ( fastQConduit
    , fastQparse
    , fastQread
    , DNAwQuality(..)
    , phred
    , illumina
    ) where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit

data DNAwQuality = DNAwQuality
            { header :: B.ByteString
            , dna_seq :: B.ByteString
            , qualities :: B.ByteString
            } deriving (Eq,Show)


fastQConduit :: (Monad m) => (Word8 -> Word8) -> Conduit B.ByteString m DNAwQuality
fastQConduit qualN = start
    where
        start = NeedInput push0 close
        push0 h = NeedInput (push1 h) close
        push1 h sq = NeedInput (push2 h sq) close
        push2 h sq _ = NeedInput (push3 h sq) close
        push3 h sq qs = HaveOutput start (return ())
                            DNAwQuality { dna_seq=sq, header=h, qualities=B.map qualN qs }
        close = Done Nothing ()

fastQparse :: (Word8 -> Word8) -> [B.ByteString] -> [DNAwQuality]
fastQparse _ [] = []
fastQparse qualN (h:sq:_:qs:rest) = (first:fastQparse qualN rest)
    where first = DNAwQuality { dna_seq=sq, header=h, qualities=B.map qualN qs }
fastQparse _ _ = error "Data.NGH.FastQ.fastQparse: incomplete record"

fastQread :: (Word8 -> Word8) -> L.ByteString -> [DNAwQuality]
fastQread qualN = fastQparse qualN . map (S.concat . L.toChunks) . L8.lines

phred :: Word8 -> Word8
phred c = c - (fromIntegral (33::Int))

illumina :: Word8 -> Word8
illumina c = c - (fromIntegral (64::Int))
