{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.NGH.Formats.Tests.FastQ
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Data.NGH.FastQ (illumina)
import Data.NGH.Formats.FastQ (fastQwrite,fastQread)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8 ()

tests = $(testGroupGenerator)

text = "\
    \@SRR065639.1 SOLEXAWS1_0001:1:1:4:1261 length=75\n\
    \TCATGGACGAGAAGATAAAGTAANNANNNNAGGAGAAGNNNANNNAANTAATGGACNNNAANNNNNAGTAAGTAA\n\
    \+\n\
    \9BC?C?C?ABBBCBB?CCBB;BC!!B!!!!@BAB####!!!#!!!##!########!!!##!!!!!#########\n\
    \@SRR065639.2 SOLEXAWS1_0001:1:1:4:631 length=75\n\
    \ATTCTTCTTCTTTTTGTAGTCGTNNTNNNNCTTGTCTTNNNANNNCTNCTTCTTTTNNNTGNNNNNCTTGTCTTT\n\
    \+\n\
    \?57?:B:@5.1BA<=C<A6<:##!!#!!!!########!!!#!!!##!########!!!##!!!!!#########\n\
    \@SRR065639.3 SOLEXAWS1_0001:1:1:4:456 length=75\n\
    \GAAATTCAAGTAGTACAGAAGATNNTNNNNAAGTCCCANNNANNNCTNTTGTAATANNNGANNNNNATAAAGTAA\n\
    \+\n\
    \CCCCB@?CCB>CB=CACCCCCC>!!@!!!!CBB#####!!!#!!!##!########!!!##!!!!!#########\n"

strict = S.concat . L.toChunks

case_read_write = (strict . L.concat . map (fastQwrite illumina) . fastQread illumina) text @?= strict text

case_read = (length . fastQread illumina) text @?= 3

