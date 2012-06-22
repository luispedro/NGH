module Data.NGH.FastQ
    ( DNAwQuality(..)
    , phred
    , illumina
    , guess_format
    ) where

import Data.Word
import qualified Data.ByteString as B

data DNAwQuality = DNAwQuality
            { header :: B.ByteString
            , dna_seq :: B.ByteString
            , qualities :: B.ByteString
            } deriving (Eq,Show)

phred :: Word8
phred = fromIntegral (33::Int)

illumina :: Word8
illumina = fromIntegral (64::Int)

guess_format :: [Word8] -> Word8
guess_format qs
    | any (< (fromIntegral (64 :: Int))) qs = phred
    | otherwise = illumina
