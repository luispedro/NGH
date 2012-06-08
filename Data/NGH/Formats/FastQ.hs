module Data.NGH.Formats.FastQ
    ( fastQConduit
    , fastQparse
    , fastQread
    , fastQwrite
    ) where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.NGH.FastQ
import Data.Conduit
import Control.Monad
import qualified Data.Conduit.Binary as CB

-- | fastQConduit is a Conduit from B.ByteString to DNAwQuality
fastQConduit :: (Monad m) => (Word8 -> Word8) -> Conduit B.ByteString m DNAwQuality
fastQConduit q = CB.lines =$= fastQConduit' q

fastQConduit' :: (Monad m) => (Word8 -> Word8) -> Conduit B.ByteString m DNAwQuality
fastQConduit' qualN = toPipe . forever $ do
    h <- await
    sq <- await
    void await
    qs <- await
    yield $ DNAwQuality { dna_seq = sq, header = h, qualities = B.map qualN qs }

-- | fastQparse read a list of lines and returns a lazy list of DNAwQuality
fastQparse :: (Word8 -> Word8) -> [B.ByteString] -> [DNAwQuality]
fastQparse _ [] = []
fastQparse qualN (h:sq:_:qs:rest) = (first:fastQparse qualN rest)
    where first = DNAwQuality { dna_seq=sq, header=h, qualities=B.map qualN qs }
fastQparse _ _ = error "Data.NGH.FastQ.fastQparse: incomplete record"

fastQread :: (Word8 -> Word8) -> L.ByteString -> [DNAwQuality]
fastQread qualN = fastQparse qualN . map (S.concat . L.toChunks) . L8.lines


-- | fastQwrite is to write in FastQ format. It does no IO, but formats it as a string
fastQwrite :: DNAwQuality -> L.ByteString
fastQwrite s = L.fromChunks
                    [header s
                    ,S8.pack "\n"
                    ,dna_seq s
                    ,S8.pack "\n+\n"
                    ,encodeQ $ qualities s
                    ,S8.pack "\n"]

encodeQ = S.map (+ (fromIntegral (64::Int)))
