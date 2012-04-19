{-# LANGUAGE BangPatterns #-}
module Data.NGH.SuffixTrie
    ( buildTrie
    , Node(..)
    , walk
    , _prettyprint
    , _root
    ) where


import Data.Maybe
import Data.Word
import Data.List
import qualified Data.ByteString as B
import Data.Convertible

data Node = Node
            { _pos :: !Int
            , _sdepth :: !Int
            , _children :: [(Word8,Node)]
            , _slink :: Node
            } deriving (Eq)

instance Show Node where
    show n = concat [
                "Node { pos = ", show (_pos n),
                        ", sdepth = ", show (_sdepth n),
                        ", children = ", show (_children n),
                "}"]

newtype Trie = Trie Node deriving (Show)
_root :: Trie -> Node
_root (Trie n) = n

down :: Word8 -> Node -> Maybe Node
down v = (lookup v) . _children

buildTrie :: B.ByteString -> Trie
buildTrie s = Trie root
    where
        len = B.length s
        root = Node { _pos=0, _sdepth=0, _children=(children 0 [] [0..(len-1)]), _slink=root }
        children :: Int -> [Word8] -> [Int] -> [(Word8, Node)]
        children _ _ [] = []
        children sd path ss = [(first, Node
                                        (head next)
                                        (sd+1)
                                        (children (sd+1) (path ++ [first]) next)
                                        (findSlink (path ++ [first]))) |
                                    (first,next) <- map (\xs -> ( fst (head xs), map snd xs))
                                                    $ groupBy (\a b -> (fst a) == (fst b))
                                                    $ sortBy (\a b -> (fst a) `compare` (fst b))
                                                    $ map (\p -> (s `B.index` (p+sd), p))
                                                    $ filter (\p -> (p+sd) < len)
                                                    ss]
        findSlink ps = fromJust $ downmany (tail ps) root
        downmany [] n = Just n
        downmany (p:ps) n = (down p n) >>= (downmany ps)


walk :: Trie -> B.ByteString -> [(Int,Int,Int)]
walk (Trie root) b = walk' root 0
    where
        walk' n@(Node p sd _ slink) bi
            | bi == (B.length b) = here
            |otherwise = case down (b `B.index` bi) n of
                    Just next -> walk' next (bi+1)
                    Nothing -> if at_root
                                    then walk' n (bi+1)
                                    else (here ++ walk' slink bi)
            where
                here = if at_root
                            then []
                            else [(bi,p,sd)]
                at_root = sd == 0

_prettyprint :: Trie -> [String]
_prettyprint (Trie root) = pp' 0 (_children root)
    where
        pp' :: Int -> [(Word8, Node)] -> [String]
        pp' _ [] = []
        pp' p ((w,n):cs) = [pref ++ w'] ++ (pp' (p+1) (_children n)) ++ (pp' p cs)
            where
                pref = take p (repeat ' ')
                w' :: [Char]
                w' = [convert w]

