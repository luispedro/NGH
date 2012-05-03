module Data.NGH.Alignments
    ( SamLine(..)
    , isAligned
    ) where

import qualified Data.ByteString as S
import Data.Bits (testBit)

data SamLine = SamLine
            { samQName :: S.ByteString
            , samFlag :: Int
            , samRName :: S.ByteString
            , samPos :: Int
            , samMapq :: Int
            , samCigar :: S.ByteString
            , samRNext :: S.ByteString
            , samPNext :: Int
            , samTLen :: Int
            , samSeq :: S.ByteString
            , samQual :: S.ByteString
            } deriving (Eq, Show)

isAligned :: SamLine -> Bool
isAligned = not . (`testBit` 2) . samFlag

