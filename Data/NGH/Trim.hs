module Data.NGH.Trim
    ( trim
    , trimpoints
    ) where
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.NGH.FastQ

-- move this up the namespace ladder
(!) = (V.!)

-- | this should be a part of the ByteString package, but it isn't.
bSlice start end = (B.take (end-start)) . (B.drop start)


trimstart qs qualthresh = trimstart' (min (n-1) 4)
    where
        n = V.length qs
        trimstart' 0 = 0
        trimstart' n
            | (qs ! n) >= qualthresh = trimstart' (n-1)
            | otherwise = (n+1)

trimend qs qualthresh = trimend' (n-1)
    where
        n = V.length qs
        trimend' 0 = 0
        trimend' n
            | (qs ! n) >= qualthresh = (n+1)
            | otherwise = trimend' (n-1)

trimpoints qs qualthresh = (trimstart qs qualthresh, trimend qs qualthresh)
trim sq@DNAwQuality {dna_seq=seq,qualities=qs} qualthresh = sq{dna_seq=bSlice st e seq, qualities=V.slice st e qs}
    where
        (st,e) = trimpoints qs qualthresh
