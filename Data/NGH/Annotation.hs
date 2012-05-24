module Data.NGH.Annotation
    ( GffLine(..)
    , GffType(..)
    , GffStrand(..)
    , gffGeneId
    , intervals
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.IntervalMap.FingerTree as IM
import Data.Maybe (fromMaybe)
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

parseGffAttributes :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseGffAttributes = map (\(aid,aval) -> (aid,S.tail aval))
                        . map (S8.break (=='='))
                        . S8.split ';'

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

gffGeneId g = lookup (S8.pack "ID") (parseGffAttributes $ gffAttributes g)

intervals :: [GffLine] -> IM.IntervalMap Int S.ByteString
intervals = foldl insertg IM.empty
    where
        insertg im g = IM.insert (asInterval g) (fromMaybe (S8.pack "unknown") $ gffGeneId g) im
        asInterval g = IM.Interval (gffStart g) (gffEnd g)
