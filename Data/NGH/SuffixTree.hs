{-# LANGUAGE TypeFamilies, FlexibleContexts, BangPatterns #-}
module Data.NGH.SuffixTree
    ( buildTree
    , prettyprint
    , walk
    , Node(..)
    , _nodepos
    , _root
    )
    where

import Data.Maybe
import Data.Word
import Data.List
import qualified Data.ByteString as B

import Control.Exception (assert)

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
            , _root :: Node
            } deriving (Eq, Show)

data Node = Leaf !Int
            | Inner
                { _pos :: !Int
                , _sdepth :: !Int
                , _children :: [Node]
                , _slink :: Node
                }
            deriving (Eq)

instance Show Node where
    show (Leaf p) = concat ["Leaf ", show p]
    show (Inner p sd cs _) = concat ["Inner ", show p, " ", show sd, " [", concatMap ((++", ") . show) cs, "]"]


selectChild :: (GenSeq a, Eq (Base a)) => a -> Node -> Base a -> Maybe Node
selectChild rseq n v = selectChild' (_children n)
    where
        sd = _sdepth n
        selectChild' [] = Nothing
        selectChild' (c:cs)
            | (rseq `seqindex` (_nodepos c + sd)) == v = Just c
            | otherwise = selectChild' cs

_nodepos :: Node -> Int
_nodepos (Leaf p) = p
_nodepos Inner{_pos=p} = p

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
    | itd == (_sdepth n) = case selectChild (raw_seq st) n c of
                    Nothing -> Nothing
                    Just next -> Just sti{node=next, itdepth=(itd+1), parent=n}
    | c == (raw_seq st `seqindex` ((_pos n)+itd)) = Just sti{itdepth=(itd+1)}
    | otherwise = Nothing


followSlink :: (GenSeq a, Eq (Base a)) => STIterator a -> STIterator a
followSlink sti@STIterator{tree=st, node=nd, parent=par, itdepth=itd}
    | (_sdepth nd) == 0 = assert (itd == 0) sti { parent=nd }
    | otherwise = fromJust $ downmany s sti{node=next, parent=undefined, itdepth=(assert (start < itd) start)}
    where
        orig_pos = _nodepos nd
        next = _slink par
        start = _sdepth next
        rseq = raw_seq st
        s = (orig_pos+1) + start
        downmany p i
            | (itdepth i) == (itd-1) = Just i
            | otherwise = (down i (rseq `seqindex` p)) >>= (downmany (p+1))


rootiterator :: STree a -> STIterator a
rootiterator st = STIterator { tree=st, node=(_root st), parent=(_root st), itdepth=0 }

walkit :: (GenSeq a, Eq (Base a)) => STIterator a -> [Base a] -> [(Int,Int)]
walkit sti@STIterator{node=n,itdepth=itd} cc
    | null cc = here
    | otherwise  = case down sti c of
        Just next -> walkit next cs
        Nothing -> (here ++ rest)
    where
        (c:cs) = cc
        here = if at_root then [] else [(_nodepos n, itd)]
        at_root = itd == 0
        rest = walkit (followSlink sti) (if at_root then cs else cc)

walk :: (GenSeq a, Eq (Base a)) => STree a -> [Base a] -> [(Int,Int)]
walk = walkit . rootiterator

buildTree :: (Ord (Base a), GenSeq a, Eq (Base a)) => a -> STree a
buildTree rseq = STree rseq r
    where
        n = seqlen rseq
        r = putsuffixes r 0 [0..(n-1)]
        putsuffixes _ _ [] = error "putsuffixes []"
        putsuffixes _ _ [s] = Leaf s
        putsuffixes par sd ss@(f:_) =  ret
            where
                ret = Inner f sd next slinknode
                slinknode = if sd == 0 then r
                            else findSlink (_slink par)

                findSlink nd
                    | (_sdepth nd) == (sd-1) = nd
                    | otherwise = findSlink (fromJust $ selectChild rseq nd (rseq `seqindex` (f+(_sdepth nd)+1)))

                next = [putsuffixes ret (newsd 0 gs) gs | gs <- (
                                        (groupBy (\a b -> (comparefst a b) == EQ)) ((sortBy comparefst) ss)
                                        )]
                newsd i gs = if allEq [rseq `seqindex` (sd+i+g)|g<-gs] then
                            newsd (i+1) gs
                        else (sd+i)
                allEq [] = True
                allEq (x:xs) = all (==x) xs
                comparefst :: Int -> Int -> Ordering
                comparefst p0 p1 = (at p0) `compare` (at p1)
                    where at p = (rseq `seqindex` (p+sd))

prettyprint :: (GenSeq a) => STree a -> [a]
prettyprint st = pretty 0 (_root st)
    where
        rseq = raw_seq st
        n = seqlen rseq
        pretty d (Leaf p) = [seqslice (p+d) n rseq]
        pretty _ (Inner p sd cs _) = ((seqslice p (p+sd) rseq):concat [pretty sd c | c <- cs])

