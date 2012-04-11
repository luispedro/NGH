{-# LANGUAGE OverloadedStrings #-}

module Data.NGH.FastQ
    ( fastQConduit
    , DNAwQuality(..)
    ) where

import Data.Word
import Data.Char
import Data.Convertible
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Conduit

data DNAwQuality = DNAwQuality
            { header :: B.ByteString
            , dna_seq :: B.ByteString
            , qualities :: (V.Vector Word8)
            } deriving (Eq,Show)


newtype FQBuffer = FQBuffer [B.ByteString]

fastQConduit :: (Monad m) => Conduit B.ByteString m DNAwQuality
fastQConduit = conduitState init push close
    where
        push (FQBuffer buf) line = return (if length buf == 3
                            then StateProducing init [one]
                            else StateProducing (FQBuffer (line:buf)) []
            ) where
                one = DNAwQuality { dna_seq=seq, header=header, qualities=q line }
                [_plus,seq,header] = buf
                q = V.fromList . (map qualN) . B.unpack
        close _ = return []
        init = FQBuffer []

ord8 :: Char -> Word8
ord8 = convert . ord
qualN c | c > ord8 'A' = c - (ord8 'A')
qualN c = (-1)
