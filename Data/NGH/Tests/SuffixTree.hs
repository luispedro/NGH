{-# LANGUAGE TemplateHaskell #-}
module Data.NGH.Tests.SuffixTree
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Data.NGH.SuffixTree

tests = $(testGroupGenerator)

s = "mississippi"
t = buildTree (s ++ ['$'])

case_perfect = (length (walk t s)) @?= 1

case_none = (length (walk t s')) @?= 0
    where s' = "MISSISSIPPI"

case_some = (length (walk t s')) @?= 0
    where s' = "misasaasassas"
