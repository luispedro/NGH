{-# LANGUAGE TemplateHaskell #-}
module Data.NGH.Tests.Align
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import qualified Data.Vector as V

import Data.NGH.Align

tests = $(testGroupGenerator)

c :: String -> String -> String
c a b = local_align (V.fromList a) (V.fromList b) 1

case_eq = (c a a) @?= (map (const 'M') a)
    where a = "ABCDEFG"

case_neq = (c a b) @?= "MMXMM"
    where
        a = "AAAAA"
        b = "AABAA"

