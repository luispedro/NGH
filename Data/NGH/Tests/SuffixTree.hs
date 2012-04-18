{-# LANGUAGE TemplateHaskell #-}
module Data.NGH.Tests.SuffixTree
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Data.NGH.SuffixTree

tests = $(testGroupGenerator)

case_perfect = (length (walk t s)) @?= 1
    where
        s = "mississippi"
        t = buildTree (s ++ ['$'])

case_none = (length (walk t s')) @?= 0
    where
        s' = "MISSISSIPPI"
        s = "mississippi"
        t = buildTree (s ++ ['$'])
