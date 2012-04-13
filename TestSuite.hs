module Main where

import Test.Framework
import Data.NGH.Tests.Align

main = defaultMain [
    Data.NGH.Tests.Align.tests
    ]
