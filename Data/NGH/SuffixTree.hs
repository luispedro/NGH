{-# LANGUAGE TypeFamilies, FlexibleContexts, BangPatterns #-}
module Data.NGH.SuffixTree
    (
    )
    where

import Data.Maybe
import Data.Word
import Data.List
import qualified Data.ByteString as B

class GenSeq as where
    type Base as :: *
    seqlen :: as -> Int
    seqindex :: as -> Int -> Base as
    seqslice :: Int -> Int -> as -> as
    seqToList :: as -> [Base as]

instance GenSeq [a] where
    type Base [a] = a
    seqlen = length
    seqindex = (!!)
    seqslice from to = (take (to-from)) . (drop from)
    seqToList = id

instance GenSeq (B.ByteString) where
    type Base B.ByteString = Word8
    seqlen = B.length
    seqindex = B.index
    seqslice from to = (B.take (to-from)) . (B.drop from)
    seqToList = B.unpack

data STree s = STree
            { raw_seq :: s
            , root :: Node
            } deriving (Eq, Show)

data Node = Leaf !Int
            | Inner
                { pos :: !Int
                , sdepth :: !Int
                , children :: [Node]
                , slink :: Node
                }
            deriving (Eq)

instance Show Node where
    show (Leaf p) = concat ["Leaf ", show p]
    show (Inner p sd cs _) = concat ["Inner ", show p, " ", show sd, " [", concatMap ((++", ") . show) cs, "]"]


selectChild :: (GenSeq a, Eq (Base a)) => a -> Node -> Base a -> Maybe Node
selectChild seq n v = selectChild' (children n)
    where
        sd = sdepth n
        selectChild' [] = Nothing
        selectChild' (c:cs)
            | (seq `seqindex` (nodepos c + sd)) == v = Just c
            | otherwise = selectChild' cs

nodepos (Leaf p) = p
nodepos Inner{pos=p} = p

data STIterator = AtLeaf
                    { leafPos :: !Int
                    , alongLeaf :: !Int
                    , parentNode :: Node
                    }
                | AtInner
                    { atNode :: Node
                    , alongNode :: !Int
                    }
    deriving (Eq, Show)

down :: (GenSeq a, Eq (Base a)) => STree a -> STIterator -> Base a -> Maybe STIterator
down st at@AtLeaf{leafPos=lp,alongLeaf=al} c
    | (c `asTypeOf` c') == c' = Just at{alongLeaf=(al+1)}
    | otherwise = Nothing
    where
        c' =  (rseq `seqindex` ap)
        rseq = raw_seq st
        ap :: Int
        ap = lp + al


down st ai@AtInner{atNode=n, alongNode=np} c
    | np == (sdepth n) = case selectChild (raw_seq st) n c of
                    Nothing -> Nothing
                    Just l@(Leaf lp) -> Just (AtLeaf lp (np+1) n)
                    Just next -> Just (AtInner next (np+1))
    | c == (raw_seq st `seqindex` ((pos n)+np)) = Just ai {alongNode=(np+1)}
    | otherwise = Nothing

downmany :: (GenSeq a, Eq (Base a)) => STree a -> STIterator -> [Base a] -> Maybe STIterator
downmany st it [] = Just it
downmany st it (c:cs) = (down st it c) >>= (\it' -> downmany st it' cs)

followSlink :: (GenSeq a, Eq (Base a)) => STree a -> STIterator -> STIterator
followSlink st at@AtLeaf{alongLeaf=lp, parentNode=n}
        = fromJust $ downmany st (AtInner next start) (seqToList cs)
    where
        next = slink n
        start = sdepth next
        cs = seqslice s e (raw_seq st)
        s = (pos next) + start
        e = (pos next) + (lp-1)

followSlink st ai@AtInner{alongNode=an, atNode=n}
    | (sdepth n) == 0 = ai
    | otherwise = AtInner { atNode=(slink n), alongNode=(an-1)}

rootiterator :: STree a -> STIterator
rootiterator st = AtInner (root st) 0

walk :: (GenSeq a, Eq (Base a)) => STree a -> STIterator -> [Base a] -> [Int]
walk st !sti [] = []
walk st sti cc@(c:cs) = case down st sti c of
        Just n -> if null cs then [posof n]
                    else walk st n cs
        Nothing -> ((posof sti):(walk st (followSlink st sti) (if posof sti == 0 then cs else cc)))

posof at@AtLeaf { leafPos=p } = p
posof ai@AtInner { atNode=n } = pos n

buildTree seq = STree seq root
    where
        n = seqlen seq
        root = putsuffixes root 0 [0..(n-1)]
        putsuffixes _ _ [] = error "putsuffixes []"
        putsuffixes _ _ [s] = Leaf s
        putsuffixes parent sd ss@(f:_) =  ret
            where
                ret = Inner f sd next slinknode
                slinknode = findSlink (slink parent) f sd

                findSlink _ _ sd | sd == 0 = root
                findSlink nd@(Inner nP nSd nCh _) f sd
                    | nSd == (sd-1) = nd
                    | otherwise = findSlink (fromJust $ selectChild seq nd (seq `seqindex` (f+nSd+1))) f sd
                findSlink _ _ _ = error "Should not have happened"

                next = [putsuffixes ret (newsd 0 gs) gs | gs <- (
                                        (groupBy (\a b -> (comparefst a b) == EQ)) ((sortBy comparefst) ss)
                                        )]
                newsd i group = if allEq [seq `seqindex` (sd+i+g)|g<-group] then
                            newsd (i+1) group
                        else (sd+i)
                allEq [] = True
                allEq (x:xs) = all (==x) xs
                comparefst :: Int -> Int -> Ordering
                comparefst p0 p1 = (at p0) `compare` (at p1)
                    where at p = (seq `seqindex` (p+sd))

prettyprint st = pretty 0 (root st)
    where
        seq = raw_seq st
        n = seqlen seq
        pretty d (Leaf p) = [seqslice (p+d) n seq]
        pretty d (Inner p sd cs _) = ((seqslice p (p+sd) seq):concat [pretty sd c | c <- cs])

