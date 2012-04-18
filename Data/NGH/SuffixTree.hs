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

data STIterator a = STIterator
                    { tree :: STree a
                    , node :: Node
                    , parent :: Node
                    , itdepth :: !Int
                    } deriving (Eq, Show)

down :: (GenSeq a, Eq (Base a)) => STIterator a -> Base a -> Maybe (STIterator a)
down sti@STIterator{tree=st, node=(Leaf p), itdepth=itd} c
    | c == c' = Just sti{ itdepth=(itd+1) }
    | otherwise = Nothing
    where
        c' =  (rseq `seqindex` (itd+p))
        rseq = raw_seq st


down sti@STIterator{tree=st, node=n, itdepth=itd} c
    | itd == (sdepth n) = case selectChild (raw_seq st) n c of
                    Nothing -> Nothing
                    Just next -> Just sti{node=next, itdepth=(itd+1), parent=n}
    | c == (raw_seq st `seqindex` ((pos n)+itd)) = Just sti{itdepth=(itd+1)}
    | otherwise = Nothing


followSlink :: (GenSeq a, Eq (Base a)) => STIterator a -> STIterator a
followSlink sti@STIterator{tree=st, node=(Leaf p), parent=n, itdepth=itd}
        = fromJust $ downmany s sti{node=next, parent=undefined, itdepth=start}
    where
        next = slink n
        start = sdepth next
        rseq = raw_seq st
        s = (pos next) + start
        e = (pos next) + (itd-1)
        downmany p sti
            | p == e = Just sti
            | otherwise = (down sti (rseq `seqindex` p)) >>= (downmany (p+1))

followSlink sti@STIterator{node=n, itdepth=itd}
    | (sdepth n) == 0 = sti
    | otherwise = sti { node=(slink n), parent=undefined, itdepth=(itd-1) }

rootiterator :: STree a -> STIterator a
rootiterator st = STIterator { tree=st, node=(root st), parent=(root st), itdepth=0 }

walk :: (GenSeq a, Eq (Base a)) => STIterator a -> [Base a] -> [Int]
walk STIterator{node=n} [] = [nodepos n]
walk sti@STIterator{node=n} cc@(c:cs) = case down sti c of
        Just next -> walk next cs
        Nothing -> ((nodepos n):(walk (followSlink sti) (if nodepos n == 0 then cs else cc)))

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

