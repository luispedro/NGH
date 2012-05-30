{-# LANGUAGE BangPatterns #-}

module Data.NGH.Trim
    ( trim
    , trimLS
    , trim_adapter
    , trim_exact_adapter
    , bSlice
    ) where
import qualified Data.ByteString as B
import qualified Data.ByteString as S
import Data.List
import Data.Maybe
import Data.Word

import Data.NGH.FastQ
import Data.NGH.SuffixTrie

-- | this should be a part of the ByteString package, but it isn't.
bSlice :: Int -> Int -> B.ByteString -> B.ByteString
bSlice start end = (B.drop start) . (B.take end)


-- | trim to longest substring
trimLS :: Word8 -> DNAwQuality -> DNAwQuality
trimLS qualthresh sqq@DNAwQuality {dna_seq = sq, qualities = qs} = sqq{dna_seq=bSlice st e sq, qualities=bSlice st e qs}
    where
        n = S.length qs
        below = S.findIndices (< qualthresh) qs
        st = st' + 1
        (st',e) = case below of
                [] -> (0,n)
                [p] -> if p > n - p
                            then (0,p)
                            else (p+1,n)
                ps0@(_:ps1) -> maximumBy (\a b -> (sz a) `compare` (sz b)) $ zip ps0 ps1
        sz (a,b) = b-a

trimstart :: B.ByteString -> Word8 -> Int
trimstart qs qualthresh = fromMaybe 0 $ B.findIndex (>= qualthresh) qs

trimend :: B.ByteString -> Word8 -> Int
trimend qs qualthresh = trimend' (n-1)
    where
        n = B.length qs
        trimend' 0 = 0
        trimend' !p
            | (qs `B.index` p) >= qualthresh = (p+1)
            | otherwise = trimend' (p-1)


trim :: DNAwQuality -> Word8 -> DNAwQuality
trim sqq@DNAwQuality {dna_seq=sq,qualities=qs} qualthresh = sqq{dna_seq=bSlice st e sq, qualities=bSlice st e qs}
    where
        st = trimstart qs qualthresh
        e  = trimend qs qualthresh

trim_exact_adapter :: B.ByteString -> Int -> B.ByteString -> B.ByteString
trim_exact_adapter adapter minhit = perform
    where
        t = buildTrie (adapter `B.snoc` 0)
        perform :: B.ByteString -> B.ByteString
        perform s = removehit minhit (qi-len) len s
            where (qi, _, len) = maximumBy (\(_,_, d) (_,_, d') -> d `compare` d') $ walk t s
            -- where (qi, _, len) = head $ walk t s

removehit :: Int -> Int -> Int -> B.ByteString -> B.ByteString
removehit minhit start len sq = if len < minhit then sq else bSlice s e sq
    where
        len_keep_start = start
        len_keep_end = (B.length sq) - start - len
        (s,e) = if len_keep_start > len_keep_end
                        then (0,start)
                        else (start+len, B.length sq)

trim_adapter :: B.ByteString -> B.ByteString -> Int -> Int -> B.ByteString
trim_adapter ad sq mm minhit = removehit minhit best_si longest sq
    where
        ns = B.length sq
        na = B.length ad
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
