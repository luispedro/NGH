module Data.NGH.Trim
    ( trim
    , trimpoints
    , trim_adapter
    , bSlice
    ) where
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.List
import Data.Word

import Data.NGH.FastQ

-- move this up the namespace ladder
(!) :: V.Vector a -> Int -> a
(!) = (V.!)

-- | this should be a part of the ByteString package, but it isn't.
bSlice :: Int -> Int -> B.ByteString -> B.ByteString
bSlice start end = (B.take (end-start)) . (B.drop start)


trimstart :: V.Vector Word8 -> Word8 -> Int
trimstart qs qualthresh = trimstart' (min (n-1) 4)
    where
        n = V.length qs
        trimstart' 0 = 0
        trimstart' p
            | (qs ! p) >= qualthresh = trimstart' (p-1)
            | otherwise = (p+1)

trimend :: V.Vector Word8 -> Word8 -> Int
trimend qs qualthresh = trimend' (n-1)
    where
        n = V.length qs
        trimend' 0 = 0
        trimend' p
            | (qs ! p) >= qualthresh = (p+1)
            | otherwise = trimend' (p-1)

trimpoints :: V.Vector Word8 -> Word8 -> (Int,Int)
trimpoints qs qualthresh = (trimstart qs qualthresh, trimend qs qualthresh)

trim :: DNAwQuality -> Word8 -> DNAwQuality
trim sqq@DNAwQuality {dna_seq=sq,qualities=qs} qualthresh = sqq{dna_seq=bSlice st e sq, qualities=V.slice st e qs}
    where
        (st,e) = trimpoints qs qualthresh

trim_adapter :: B.ByteString -> B.ByteString -> Int -> Int -> B.ByteString
trim_adapter ad sq mm minhit = if longest < minhit then sq else bSlice start end sq
    where
        ns = B.length sq
        na = B.length ad
        e_5 = best_si
        s_3 = best_si + longest
        len_5 = e_5
        len_3 = ns - s_3
        (start,end) = if len_5 > len_3 then (0,e_5) else (s_3, B.length sq)
        (longest, best_si) = maximumBy (\a b -> compare (fst a) (fst b)) matches
        matches :: [(Int,Int)]
        matches = [(matchat si ai 0 0,si) | si <- [0..(ns-1)], ai <- [0..(na-1)]]
        matchat si ai j mm_sofar
            | mm_sofar > mm = (j-1)
            | (si+j) >= ns || (ai+j) >= na = j
            | otherwise = matchat si ai (j+1) (mm_sofar + neq (si+j) (ai+j))
        neq si ai = if (sq `B.index` si) /= (ad `B.index` ai)
                    then 1
                    else 0
