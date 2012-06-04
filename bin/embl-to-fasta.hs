import Data.Maybe
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.NGH.Formats.Fasta
import Data.NGH.Formats.Embl
import Codec.Compression.GZip


main :: IO ()
main = do
    [inputf,outputf] <- getArgs
    let header = L.concat [L8.pack "converted from ", L8.pack inputf]
    (L.readFile inputf) >>= (L.writeFile outputf . writeSeq 72 header . fromJust . readSeq . decompress)
    
