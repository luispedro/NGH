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


fastQConduit :: (Monad m) => Conduit B.ByteString m DNAwQuality
fastQConduit = start
    where
        start = NeedInput push0 close
        push0 h = NeedInput (push1 h) close
        push1 h sq = NeedInput (push2 h sq) close
        push2 h sq _ = NeedInput (push3 h sq) close
        push3 h sq qs = HaveOutput start (return ())
                            DNAwQuality { dna_seq=sq, header=h, qualities=q qs }
        close = Done Nothing ()
        q = V.fromList . (map qualN) . B.unpack

ord8 :: Char -> Word8
ord8 = convert . ord

qualN :: Word8 -> Word8
qualN c | c > ord8 'A' = c - (ord8 'A')
qualN _ = (-1)

