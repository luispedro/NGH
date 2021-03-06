{-# LANGUAGE TemplateHaskell #-}
module Data.NGH.Tests.Alignments
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Data.NGH.Alignments

tests = $(testGroupGenerator)

case_isAligned = isAligned (SamLine ud 16 ud 0 0 ud ud 0 0 ud ud) @? "Should be aligned"
case_isNotAligned = (not $ isAligned (SamLine ud 4 ud 0 0 ud ud 0 0 ud ud)) @? "Should not be aligned"
ud = undefined



