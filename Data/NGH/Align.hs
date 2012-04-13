module Data.NGH.Align
    ( local_align
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST

for :: Monad m => [a] -> (a -> m b) -> m ()
for = flip mapM_

absurd :: t
absurd = error "impossible situation"

local_align :: (Eq a) => V.Vector a -> V.Vector a -> Int -> [Char]
local_align seq1 seq2 d = alg
    where
        n = V.length seq1
        m = V.length seq2
        cm i j = if (seq1 V.! i) /= (seq2 V.! j) then 1 else 0
        alg = runST $ do
            let g2 v i j = (flip MV.read) j =<< MV.read v i
                w2 v i j a = do
                    vi <- MV.read v i
                    MV.write vi j a
                allocV = do
                    vR <- MV.new n
                    for [0..(m-1)] $ \i ->
                        (MV.write vR i) =<< (MV.new m)
                    return vR
            vF <- allocV
            vP <- allocV
            for ([0..(n-1)] :: [Int]) $ \i -> do
                w2 vF i 0 0
                w2 vP i 0 1
            for [0..(m-1)] $ \j -> do
                w2 vF 0 j 0
                w2 vP 0 j 2
            w2 vP 0 0 0
            for [1..(n-1)] $ \i -> do
                for [1..(m-1)] $ \j -> do
                    match <- (g2 vF (i-1) (j-1))
                    delete <- (g2 vF (i-1) j)
                    insert <- (g2 vF i (j-1))
                    let (value,op) = min3 (match + cm i j)
                                        (delete + d)
                                        (insert + d)
                    w2 vF i j value
                    w2 vP i j op
            let backtrack i j = if (i < 0) || (j < 0)
                    then return []
                    else do
                        val <- g2 vP i j
                        let (f,ni,nj) = (case val of
                                0 -> ((if (seq1 V.! i) == (seq2 V.! j) then 'M' else 'X'),(i-1), (j-1))
                                1 -> ('D',(i-1),     j)
                                2 -> ('I',    i, (j-1))
                                _ -> absurd)
                        rest <- backtrack ni nj
                        return (rest ++ [f])
            backtrack (n-1) (m-1)
min3 :: Int -> Int -> Int -> (Int,Int)
min3 m d i
    | m == mv = (mv,0)
    | d == mv = (mv,1)
    | i == mv = (mv,2)
    | otherwise = absurd
    where mv = minimum [m,d,i]
