{-# LANGUAGE OverloadedStrings #-}
module Data.NGH.Formats.Embl
    ( readSeq
    ) where

import Data.Maybe (isJust)
import Data.Word

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

-- | read a sequence from an EMBL formatted file
-- If there is no sequence, then `Nothing` is returned
readSeq :: L.ByteString -- ^ The whole file (as a lazy string)
        -> Maybe L.ByteString -- ^ A sequence if found
readSeq = readSeq' . L8.lines

readSeq' :: [L.ByteString] -> Maybe L.ByteString
readSeq' [] = Nothing
readSeq' (s:ss)
    | "SQ" `L.isPrefixOf` s = (Just . L.fromChunks . map getSeq . beforeend) ss
    | otherwise = readSeq' ss

getSeq = S.filter isACTG . S.concat . L.toChunks
isACTG :: Word8 -> Bool
isACTG = isJust . (`L.elemIndex` "actgACTG")

beforeend [] = []
beforeend (s:ss)
    | "\\\\" `L.isPrefixOf` s = []
    | otherwise = (s:beforeend ss)

