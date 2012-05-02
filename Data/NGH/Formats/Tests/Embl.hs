{-# LANGUAGE TemplateHaskell #-}
module Data.NGH.Formats.Tests.Embl
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Data.Maybe
import Data.NGH.Formats.Embl (readSeq)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

test_file = L8.pack "\
    \ID   Pf3D7_01_v3.embl; SV ; ; ; ; ; 640851 BP.\n\
    \FH   Key             Location/Qualifiers\n\
    \FH\n\
    \FT   repeat_region   1..360\n\
    \FT                   /locus_tag=\"Pfalciparum_REP_20\"\n\
    \FT                   /note=\"telomeric repeat region\"\n\
    \SQ   Sequence 640851 BP; 260441 A; 65032 C; 66352 G; 249026 T; 0 other;\n\
    \     tgaaccctaa aacctaaacc ctaaacccta aaccctgaac cctaaaccct gaaccctaaa        60\n\
    \     ccctaaaccc tgaaccctaa accctaaacc ctgaacccta aaccctgaaa cctaaaccct       120\n\
    \     aggtttaggg ttcagggttt agggtttagg gtttagggtt tagggtttag g                171\n\
    \//\n"


tests = $(testGroupGenerator)

case_read = (L.length parsed) @?= 171
    where
        parsed = fromJust $ readSeq test_file

