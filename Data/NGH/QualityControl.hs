{-# LANGUAGE BangPatterns #-}
module Data.NGH.QualityControl
    ( avgVarQualities
    ) where

import Data.NGH.FastQ
import Data.List (foldl')
import qualified Data.ByteString as S
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
