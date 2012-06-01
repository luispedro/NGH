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
readSamLine line = case L8.split '\t' line of
    [tk0,tk1,tk2,tk3,tk4,tk5,tk6,tk7,tk8,tk9,tk10] -> SamLine
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
    tokens -> error $ concat ["Expected 11 tokens, only got ", show $ length tokens,"\n\t\tLine was '", show line, "'"]

strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

