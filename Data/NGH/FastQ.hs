module Data.NGH.FastQ
    ( DNAwQuality(..)
    , phred
    , illumina
    ) where

import Data.Word
import qualified Data.ByteString as B

data DNAwQuality = DNAwQuality
            { header :: B.ByteString
            , dna_seq :: B.ByteString
            , qualities :: B.ByteString
            } deriving (Eq,Show)

phred :: Word8 -> Word8
phred c = c - (fromIntegral (33::Int))

illumina :: Word8 -> Word8
illumina c = c - (fromIntegral (64::Int))
