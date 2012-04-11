{-# LANGUAGE OverloadedStrings #-}

module Data.NGH.FastQ
    ( fastq_sink
    , DNAwQuality(..)
    ) where

import Data.Word
import Data.Char
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import Data.Conduit
import Data.Conduit.Attoparsec
import Control.Applicative ((<*))
import Control.Monad (void,liftM)

data DNAwQuality = DNAwQuality
            { header :: B.ByteString
            , dna_seq :: B.ByteString
            , qualities :: (V.Vector Word8)
            } deriving (Eq,Show)

fastq :: Parser [DNAwQuality]
fastq = do
    s <- fastq1
    e <- atEnd
    if e then return [s]
        else do
            rest <- fastq
            return (s:rest)

fastq1 :: Parser DNAwQuality
fastq1 = do
    h <- headerline
    sq <- seqlines
    void plus_sign
    qs <- readqualities (B.length sq)
    void $ (word8 eol)
    return DNAwQuality { header=h, dna_seq=sq, qualities=qs }

ord8 :: Char -> Word8
ord8 = fromInteger . toInteger . ord
eol = ord8 '\n'

line = takeTill (==eol) <* (word8 eol)
headerline = (word8 (ord8 '@') >> line)
seqlines = takeTill (== (ord8 '+')) >>= (return . joinseqs)
joinseqs :: B.ByteString -> B.ByteString
joinseqs = B.filter (/=eol)
plus_sign = string "+\n"

readqualities n = V.fromList `liftM` (qualities' n)
    where
        qualities' :: Int -> Parser [Word8]
        qualities' 0 = return []
        qualities' n = do
            c <- anyWord8
            if c == (ord8 '\n')
                then qualities' n
                else do
                    let v = qualN c
                    rest <- qualities' (n-1)
                    return (v:rest)
        qualN c | c > ord8 'A' = c - (ord8 'A')
        qualN c = (-1)

fastq_sink :: (MonadThrow m) => Sink B.ByteString m [DNAwQuality]
fastq_sink = sinkParser fastq
