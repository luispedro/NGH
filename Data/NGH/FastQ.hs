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
fastQConduit = conduitState empty push close
    where
        push (FQBuffer buf) line = return (if length buf == 3
                            then StateProducing empty [one]
                            else StateProducing (FQBuffer (line:buf)) []
            ) where
                one = DNAwQuality { dna_seq=sq, header=h, qualities=q line }
                [_plus,sq,h] = buf
                q = V.fromList . (map qualN) . B.unpack
        close _ = return []
        empty = FQBuffer []

ord8 :: Char -> Word8
ord8 = convert . ord

qualN :: Word8 -> Word8
qualN c | c > ord8 'A' = c - (ord8 'A')
qualN _ = (-1)

