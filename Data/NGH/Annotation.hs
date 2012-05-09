module Data.NGH.Annotation
    ( GffLine(..)
    , GffType(..)
    , GffStrand(..)
    ) where

import qualified Data.ByteString as S
import Control.DeepSeq

data GffType = GffExon
                | GffGene
                | GffCDS
                | GffOther S.ByteString
            deriving (Eq, Show)
data GffStrand = GffPosStrand | GffNegStrand | GffUnknownStrand | GffUnStranded
            deriving (Eq, Show, Enum)


data GffLine = GffLine
            { gffSeqId :: S.ByteString
            , gffSource :: S.ByteString
            , gffType :: GffType
            , gffStart :: Int
            , gffEnd :: Int
            , gffScore :: Maybe Float
            , gffString :: GffStrand
            , gffPhase :: Int -- ^phase: use -1 to denote .
            , gffAttributes :: S.ByteString
            } deriving (Eq,Show)

instance NFData GffLine where
    rnf gl = (gffSeqId gl) `seq`
            (gffSource gl) `seq`
            (gffType gl) `seq`
            (gffStart gl) `seq`
            (gffEnd gl) `seq`
            (gffScore gl) `deepseq`
            (gffString gl) `seq`
            (gffPhase gl) `seq`
            (gffAttributes gl) `seq`
            ()
