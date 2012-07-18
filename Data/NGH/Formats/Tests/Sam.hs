{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.NGH.Formats.Tests.Sam
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Data.Maybe
import Data.NGH.Alignments
import Data.NGH.Formats.Sam

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8 ()

test_file = "\
    \@SQ\tSN:R9\tLN:1541735\tSP: converted from Pf3D7_09_v3.embl.gz\tUR:plasmodium.fa\n\
    \@SQ\tSN:R10\tLN:1687656\tSP: converted from Pf3D7_10_v3.embl.gz\tUR:plasmodium.fa\n\
    \@SQ\tSN:R11\tLN:2038340\tSP: converted from Pf3D7_11_v3.embl.gz\tUR:plasmodium.fa\n\
    \@SQ\tSN:R12\tLN:2271494\tSP: converted from Pf3D7_12_v3.embl.gz\tUR:plasmodium.fa\n\
    \@SQ\tSN:R13\tLN:2925236\tSP: converted from Pf3D7_13_v3.embl.gz\tUR:plasmodium.fa\n\
    \@SQ\tSN:R14\tLN:3291936\tSP: converted from Pf3D7_14_v3.embl.gz\tUR:plasmodium.fa\n\
    \@PG\tID:0\tPN:TAPyR\tVN:1.2c\n\
    \SRR065639.1 SOLEXAWS1_0001:1:1:4:1261 length=75\t4\t*\t0\t0\t*\t*\t0\t0\t*\t*\n\
    \SRR065639.2 SOLEXAWS1_0001:1:1:4:631 length=75\t0\tR13\t1430385\t0\t28M1D4M1I5M1I3M1D4M1I1M1I2M1I2M\t*\t0\t0\tATTCTTCTTCTTTTTGTAGTCGTTCTTGTCTTACTCTTCTTTTTGCTTGTCTTT\t*\n\
    \SRR065639.2 SOLEXAWS1_0001:1:1:4:631 length=75\t16\tR13\t1449199\t0\t1I3M2I5M1D4M1I4M1I4M1D29M\t*\t0\t0\tAAAGACAAGCAAAAAGAAGAGTAAGACAAGAACGACTACAAAAAGAAGAAGAAT\t*\n\
    \SRR065639.2 SOLEXAWS1_0001:1:1:4:631 length=75\t16\tR13\t1449349\t0\t1I3M2I5M1D4M1I4M1I4M1D29M\t*\t0\t0\tAAAGACAAGCAAAAAGAAGAGTAAGACAAGAACGACTACAAAAAGAAGAAGAAT\t*\n\
    \SRR065639.2 SOLEXAWS1_0001:1:1:4:631 length=75\t16\tR13\t1449499\t0\t1I3M2I5M1D4M1I4M1I4M1D29M\t*\t0\t0\tAAAGACAAGCAAAAAGAAGAGTAAGACAAGAACGACTACAAAAAGAAGAAGAAT\t*\n\
    \SRR065639.3 SOLEXAWS1_0001:1:1:4:456 length=75\t4\t*\t0\t0\t*\t*\t0\t0\t*\t*\n\
    \SRR065639.4 SOLEXAWS1_0001:1:1:4:207 length=75\t4\t*\t0\t0\t*\t*\t0\t0\t*\t*\n"

bowtie_extra = "FCC0PHYACXX:7:1101:1354:2168#0/1\t4\t*\t0\t0\t*\t*\t0\t0\tAAGTTAGTTCAGTCTACATCCAGAAATGAGCAAGAGCAGCTTGGAGGTT\tabbceeecggggghiiiiiiiiiiiiiiiihiiighhiiiiiiifhicg\tYT:Z:UU\n"

alignments = readAlignments test_file

tests = $(testGroupGenerator)

case_all = (length alignments) @?= 7
case_aligned = (length $ filter isAligned alignments) @?= 4
case_flagNot4 = (null $ filter ((==4) . samFlag) $ filter isAligned alignments) @? "Should be empty"

case_bowtie = (length $ readAlignments bowtie_extra) @?= 1
