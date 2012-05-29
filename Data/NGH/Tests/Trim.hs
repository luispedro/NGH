{-# LANGUAGE TemplateHaskell #-}
module Data.NGH.Tests.Trim
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Data.NGH.FastQ
import Data.NGH.Trim
import qualified Data.ByteString as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

tests = $(testGroupGenerator)

case_full19 = trimmed @?= expected
    where
        trimmed = trim_adapter
            (BC.pack                   "ABBBBBA")
            (BC.pack "AAAAAAAAAAAAAAAAAAABBBBBA") 1 1
        expected =
            (BC.pack "AAAAAAAAAAAAAAAAAA")

case_full19exact = trimmed @?= expected
    where
        trimmed = trim_exact_adapter
            (BC.pack                   "ABBBBBA") 2
            (BC.pack "AAAAAAAAAAAAAAAAAAABBBBBA")
        expected =
            (BC.pack "AAAAAAAAAAAAAAAAAA")

case_19 = trimmed @?= expected
    where
        trimmed = trim_adapter
            (BC.pack                   "ABBBB")
            (BC.pack "AAAAAAAAAAAAAAAAAAABBBBBA") 0 1
        expected =
            (BC.pack "AAAAAAAAAAAAAAAAAA")

case_mm2 = trimmed @?= expected
    where
        trimmed = trim_adapter
            (BC.pack                   "ABBBBBA")
            (BC.pack "AAAAAAAAAAAAAAAAAAABCBBBA") 2 3
        expected =
            (BC.pack "AAAAAAAAAAAAAAAAAA")

case_trimLS = S.length tr @?= 8
    where
        tr = qualities $ trimLS testdata 8
        testdata = DNAwQuality undefined undefined qs
        qs = S.pack [8,8,8,8,0,8,8,0,8,0,
                            8,8,8,8,
                            8,8,8,8,
                      0,8,8,8,8,8,2,8,8,8,5,8,8,8,8,2,8,8]

case_bSlice = (B.length (bSlice 0 10 (BC.pack "0123456789ABCDEF"))) @?= 10
case_bSlice9 = (B.length (bSlice 1 10 (BC.pack "0123456789ABCDEF"))) @?= 9
