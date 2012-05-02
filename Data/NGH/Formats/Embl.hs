module Data.NGH.Formats.Embl
    ( readSeq
    ) where

import Data.Maybe (isJust)

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

readSeq :: L.ByteString -> Maybe L.ByteString
readSeq = readSeq' . L8.lines

readSeq' [] = Nothing
readSeq' (s:ss)
    | bsSQ `L.isPrefixOf` s = (Just . L.fromChunks . map getSeq) ss
    | otherwise = readSeq' ss

getSeq = S.filter isACTG . S.concat . L.toChunks

bsSQ = L8.pack "SQ"
bsACTG = L8.pack "actgACTG"
isACTG = isJust . (`L.elemIndex` bsACTG)
