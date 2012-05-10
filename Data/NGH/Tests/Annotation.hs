{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.NGH.Tests.Annotation
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import qualified Data.Vector as V

import Data.NGH.Annotation

import Data.NGH.Annotation
import Data.NGH.Formats.Gff
import qualified Data.IntervalMap.FingerTree as IM

import qualified Data.ByteString.Char8 as S8

test_file = "\
    \Pf3D7_01_v3\tchado\tgene\t29510\t37126\t.\t+\t.\tID=PF3D7_0100100;Name=VAR;isObsolete=false;feature_id=20734001;timelastmodified=07.09.2011+06:34:15+BST\n\
    \Pf3D7_01_v3\tchado\tgene\t38982\t40207\t.\t-\t.\tID=PF3D7_0100200;Name=RIF;isObsolete=false;feature_id=20725865;timelastmodified=07.09.2011+05:28:48+BST\n\
    \Pf3D7_01_v3\tchado\tgene\t42367\t46507\t.\t-\t.\tID=PF3D7_0100300;isObsolete=false;feature_id=20725861;timelastmodified=07.09.2011+05:28:46+BST;previous_systematic_id=PFA0015c"

annotations = readAnnotations test_file
ints = intervals . filter ((== GffGene) . gffType) $ annotations

tests = $(testGroupGenerator)

case_intervals_inside = (length $ IM.search 38985 ints) @?= 1
case_intervals_null = (length $ IM.search 38976 ints) @?= 0

