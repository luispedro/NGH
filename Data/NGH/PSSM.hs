module Data.NGH.PSSM
    ( scan
    ) where

import qualified Data.Map as M
type Probability = Float
type AACid = Char
type Seq = String
type PSSM = [M.Map AACid Probability]

score_here :: PSSM -> Seq -> Maybe Probability
score_here [] _ = Just 0.0
score_here _ [] = Nothing
score_here (p:ps) (a:as) = do
    rest <- score_here ps as
    let score_aa = M.findWithDefault (-100.0) a p
    return (score_aa + rest)


scan :: PSSM -> Seq -> [Probability]
scan p aas = case score_here p aas of
    Nothing -> []
    Just v -> v:(scan p $ tail aas)
