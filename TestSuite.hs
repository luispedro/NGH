module Main where

import Test.Framework
import Data.NGH.Tests.Align
import Data.NGH.Tests.Alignments
import Data.NGH.Tests.SuffixTree
import Data.NGH.Tests.SuffixTrie
import Data.NGH.Tests.Trim
import Data.NGH.Formats.Tests.Embl
import Data.NGH.Formats.Tests.Fasta
import Data.NGH.Formats.Tests.Sam

main = defaultMain
    [Data.NGH.Tests.Align.tests
    ,Data.NGH.Tests.Alignments.tests
    ,Data.NGH.Tests.SuffixTree.tests
    ,Data.NGH.Tests.SuffixTrie.tests
    ,Data.NGH.Tests.Trim.tests
    ,Data.NGH.Formats.Tests.Embl.tests
    ,Data.NGH.Formats.Tests.Fasta.tests
    ,Data.NGH.Formats.Tests.Sam.tests
    ]
