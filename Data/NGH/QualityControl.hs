{-# LANGUAGE BangPatterns #-}
module Data.NGH.QualityControl
    ( avgVarQualities
    , gcByPosition
    , gcCount
    , vAverage
    ) where

import Data.Word
import Data.NGH.FastQ
import Data.List (foldl')
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Vector.Unboxed as VU

-- | avgVarQualities: average & variance of quality scores by position
avgVarQualities :: [DNAwQuality] -> (VU.Vector Double, VU.Vector Double)
avgVarQualities [] = (VU.empty, VU.empty)
avgVarQualities (q:qs) = (mu, var)
    where
        mu = VU.map (/n) s
        var = VU.zipWith (\s2i mui -> (s2i/n - mui*mui)) s2 mu
        (n,s,s2) = foldl' acc (0,zeros q, zeros q) (q:qs)
        acc (!a,!as,!as2) dq = (a+1, VU.zipWith (+) as qV, VU.zipWith (+) as2 $ VU.map (**2) qV)
            where qV = VU.fromList . map fromIntegral . S.unpack . qualities $ dq
        zeros dq = VU.replicate (S.length $ dna_seq dq) (0.0 :: Double)

gcByPosition :: DNAwQuality -> VU.Vector Bool
gcByPosition = VU.map isGC . VU.fromList . S.unpack . dna_seq

isGC :: Word8 -> Bool
isGC = (`S.elem` (S8.pack "gcGC"))

gcCount :: DNAwQuality -> Int
gcCount = VU.sum . VU.map fromEnum . gcByPosition

vAverage :: [VU.Vector Int] -> VU.Vector Double
vAverage [] = VU.empty
vAverage qs0@(q0:_) = final $ go zeros 0.0 qs0
    where
        zeros = VU.replicate m 0.0
        step = 8192.0
        m = VU.length q0
        go !partial !n [] = [(n,partial)]
        go !partial !n (q:qs)
            | n == step = ((step,partial):go zeros 0 (q:qs))
            | otherwise = go (vSum partial q) (n+1) qs
        final :: [(Double, VU.Vector Double)] -> VU.Vector Double
        final is = VU.map (/n) total
            where
                (n,total) = foldl' k (0.0,zeros) is
                k (!p,!v) (p',v') = (p+p', VU.zipWith (+) v v')
        vSum a b = VU.zipWith (+) a (VU.map fromIntegral b)

