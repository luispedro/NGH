{-# LANGUAGE OverloadedStrings #-}
module Data.NGH.Formats.Fasta
    ( writeSeq
    ) where

import Data.Convertible
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 () -- import IsString instance
import qualified Data.ByteString.Lazy as L

-- |writeSeq formats a sequence in Fast-A format
writeSeq :: Int -- ^ Width of the lines to use
            -> L.ByteString -- ^ Header
            -> L.ByteString -- ^ The DNA sequence
            -> L.ByteString -- ^ The output
writeSeq lw header s = L.fromChunks ([">"] ++ L.toChunks header ++ ["\n"] ++ breakup lw s)

breakup :: Int -> L.ByteString -> [S.ByteString]
breakup n s | n < 0 = L.toChunks s
breakup n s = L.toChunks h ++ ["\n"] ++ (if L.null t then [] else breakup n t)
    where (h,t) = L.splitAt (convert n) s
