module Main where

import Test.Framework
import Data.NGH.Tests.Align
import Data.NGH.Tests.Trim

main = defaultMain [
    Data.NGH.Tests.Align.tests,
    Data.NGH.Tests.Trim.tests
    ]
