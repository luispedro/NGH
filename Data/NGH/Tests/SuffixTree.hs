{-# LANGUAGE TemplateHaskell #-}
module Data.NGH.Tests.SuffixTree
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Data.NGH.SuffixTree

tests = $(testGroupGenerator)

s = "mississippi"
t = buildTree (s ++ ['$'])

case_perfect = (length (walk t s)) @?= 1

case_none = (length (walk t s')) @?= 0
    where s' = "MISSISSIPPI"

case_some = (walk t s') @?= [(2,1,2),(2,2,1)]
    where s' = "isa"

case_some2 = (length (walk t s')) @?= 1
    where s' = "iaaaaaaa"

case_slink = (is_root `all` (map smart_slink first_level)) @? "Depth 1 nodes do not slink to root"
    where
        first_level = _children (_root t)
        is_root = (==0) . _nodepos
        smart_slink (Leaf _) = _root t
        smart_slink n = _slink n

case_nr_leaves = (length leaves) @?= ((length s) + 1)
    where
        leaves = collect (_root t)
        collect ell@(Leaf _) = [ell]
        collect n = concat (collect `map` _children n)

case_all_leaves = (all in_leaves [1..(length s)]) @? "Not all leaves were found"
    where
        in_leaves i = (Leaf i) `elem` leaves
        leaves = collect (_root t)
        collect ell@(Leaf _) = [ell]
        collect n = concat (collect `map` _children n)


newtype Dna = Dna Char
instance Show Dna where
    show (Dna c) = [c]

instance Arbitrary Dna where
    arbitrary = (elements (map Dna "actg"))

tdna = buildTree "actgactgggattgataccatgatatgggatacatag$"
prop_finds_match q = null q || (max_match q) > 0
    where
        q' = [c | (Dna c) <- q]
        max_match q = maximum [depth | (_,_,depth) <- walk tdna q']

case_ttc  = length (walk tdna "ttc") > 0 @? "Regression (found with quick check)"
