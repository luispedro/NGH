{-# LANGUAGE OverloadedStrings #-}
module Data.NGH.Formats.Gff
    ( readAnnotations
    ) where

import Data.NGH.Annotation

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

readAnnotations :: L.ByteString -> [GffLine]
readAnnotations = readAnnotations' . L8.lines

readAnnotations' :: [L.ByteString] -> [GffLine]
readAnnotations' [] = []
readAnnotations' (l:ls) = case L8.head l of
                '#' -> readAnnotations' ls
                '>' -> []
                _ -> (readLine l:readAnnotations' ls)

readLine :: L.ByteString -> GffLine
readLine line = if length tokens == 9
            then GffLine
                (strict tk0)
                (strict tk1)
                (parsegffType $ strict tk2)
                (read $ L8.unpack tk3)
                (read $ L8.unpack tk4)
                (score tk5)
                (strand $ L8.head tk6)
                (phase tk7)
                (strict tk8)
            else error (concat ["unexpected line in GFF: ", show line])
    where
        tokens = L8.split '\t' line
        [tk0,tk1,tk2,tk3,tk4,tk5,tk6,tk7,tk8] = tokens
        parsegffType "exon" = GffExon
        parsegffType "gene" = GffGene
        parsegffType "CDS" = GffCDS
        parsegffType t = GffOther t
        score "." = Nothing
        score v = Just (read $ L8.unpack v)
        strand '.' = GffUnStranded
        strand '+' = GffPosStrand
        strand '-' = GffNegStrand
        strand '?' = GffUnknownStrand
        strand _ = error "unhandled value for strand"
        phase "." = -1
        phase r = read (L8.unpack r)


strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks


