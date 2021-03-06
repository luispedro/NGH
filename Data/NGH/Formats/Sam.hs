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


readInt b = case L8.readInt b of
    Just (v,_) -> v
    _ -> error $ concat ["Expected int, got ", show b]

readSamLine :: L.ByteString -> SamLine
readSamLine line = case L8.split '\t' line of
    (tk0:tk1:tk2:tk3:tk4:tk5:tk6:tk7:tk8:tk9:tk10:_) -> SamLine
                (strict tk0)
                (readInt tk1)
                (strict tk2)
                (readInt tk3)
                (readInt tk4)
                (strict tk5)
                (strict tk6)
                (readInt tk7)
                (readInt tk8)
                (strict tk9)
                (strict tk10)
    tokens -> error $ concat ["Expected 11 tokens, only got ", show $ length tokens,"\n\t\tLine was '", show line, "'"]

strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

