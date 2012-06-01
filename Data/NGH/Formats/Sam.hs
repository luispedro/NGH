{-# LANGUAGE OverloadedStrings #-}
module Data.NGH.Formats.Sam
    ( readAlignments
    , readSamLine
    ) where

import Data.NGH.Alignments

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

readAlignments :: L.ByteString -> [SamLine]
readAlignments = readAlignments' . L8.lines

readAlignments' :: [L.ByteString] -> [SamLine]
readAlignments' [] = []
readAlignments' (l:ls)
    | L8.head l == '@' = readAlignments' ls
    | otherwise = (readSamLine l:readAlignments' ls)

readSamLine :: L.ByteString -> SamLine
readSamLine line = SamLine
                (strict tk0)
                (read $ L8.unpack tk1)
                (strict tk2)
                (read $ L8.unpack tk3)
                (read $ L8.unpack tk4)
                (strict tk5)
                (strict tk6)
                (read $ L8.unpack tk7)
                (read $ L8.unpack tk8)
                (strict tk9)
                (strict tk10)
    where
        [tk0,tk1,tk2,tk3,tk4,tk5,tk6,tk7,tk8,tk9,tk10] = L8.split '\t' line

strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

