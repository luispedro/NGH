{-# LANGUAGE TemplateHaskell #-}
module Data.NGH.Tests.SuffixTrie
    ( tests ) where


import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Data.NGH.SuffixTrie
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

tests = $(testGroupGenerator)

s = BC.pack "mississippi$"
t = buildTrie s
root = _root t

case_perfect = (length (walk t s)) @?= 1

case_none = (length (walk t s')) @?= 0
    where s' = BC.pack "MISSISSIPPI"

case_some = (walk t s') @?= [(2,1,2),(2,2,1)]
    where s' = BC.pack "isa"

case_some2 = (length (walk t s')) @?= 1
    where s' = BC.pack "iaaaaaaa"

case_nr_leaves = leaves @?= (B.length s)
    where
        is_leaf = null . _children
        as_int False = 0
        as_int True = 1
        leaves = sum $ map (as_int . is_leaf) $ walktree root
        walktree n = (n:concat [walktree c | (_,c) <- _children n])


newtype Dna = Dna Char
instance Show Dna where
    show (Dna c) = [c]

instance Arbitrary Dna where
    arbitrary = (elements (map Dna "actg"))

tdna = buildTrie $ BC.pack "actgactgggattgataccatgatatgggatacatag"

case_find_a = length (walk tdna (BC.pack "a")) @?= 1
case_find_t = length (walk tdna (BC.pack "t")) @?= 1
case_find_c = length (walk tdna (BC.pack "c")) @?= 1
case_find_g = length (walk tdna (BC.pack "g")) @?= 1
case_dna4 = (length . _children . _root) tdna @?= 4


prop_finds_match q = null q || max_match > 0
    where
        q' = BC.pack [c | (Dna c) <- q]
        max_match = maximum [depth | (_,_,depth) <- walk tdna q']

case_ttc  = length (walk tdna (BC.pack "ttc")) > 0 @? "Regression (found with quick check)"
