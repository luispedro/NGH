module Main where

import Test.Framework
import Data.NGH.Tests.Align
import Data.NGH.Tests.Align
import Data.NGH.Tests.SuffixTree
import Data.NGH.Tests.SuffixTrie
import Data.NGH.Tests.Trim
import Data.NGH.Formats.Tests.Embl

main = defaultMain [
    Data.NGH.Tests.Align.tests,
    Data.NGH.Tests.SuffixTree.tests,
    Data.NGH.Tests.SuffixTrie.tests,
    Data.NGH.Tests.Trim.tests,
    Data.NGH.Formats.Tests.Embl.tests
    ]
