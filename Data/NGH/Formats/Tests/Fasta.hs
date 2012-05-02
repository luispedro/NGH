{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.NGH.Formats.Tests.Fasta
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Data.NGH.Formats.Fasta (writeSeq)

import qualified Data.ByteString.Lazy.Char8 as L8

tests = $(testGroupGenerator)

case_write = (length $ L8.lines $ formatted) @?= 3
    where
        formatted = writeSeq 4 "header" "actgact"

