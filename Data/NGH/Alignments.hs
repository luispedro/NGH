{-# LANGUAGE BangPatterns #-}

module Data.NGH.Alignments
    ( SamLine(..)
    , isAligned
    ) where

import qualified Data.ByteString as S
import Data.Bits (testBit)
import Control.DeepSeq

data SamLine = SamLine
            { samQName :: S.ByteString
            , samFlag :: !Int
            , samRName :: S.ByteString
            , samPos :: !Int
            , samMapq :: !Int
            , samCigar :: S.ByteString
            , samRNext :: S.ByteString
            , samPNext :: !Int
            , samTLen :: !Int
            , samSeq :: S.ByteString
            , samQual :: S.ByteString
            } deriving (Eq, Show)

isAligned :: SamLine -> Bool
isAligned = not . (`testBit` 2) . samFlag

instance NFData SamLine where
    rnf (SamLine qn f r p m c rn pn tl s qual) = qn `seq` f `seq` r `seq` p `seq` m `seq` c `seq` rn `seq` pn `seq` tl `seq` s `seq` qual `seq` ()
