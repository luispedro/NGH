{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import Data.Maybe
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.NGH.Formats.Fasta
import Data.NGH.Formats.Embl
import Codec.Compression.GZip

data E2FCmd = E2FCmd
        { input :: String
        , output :: String
        } deriving (Eq, Show, Data, Typeable)

e2fcmds = E2FCmd
        { input = "-" &= argPos 0 &= typ "Input-file"
        , output = "-" &= argPos 1 &= typ "Output-file"
        } &=
        verbosity &=
        details ["Convert Embl to Fasta files"]

main :: IO ()
main = do
    E2FCmd inputf outputf <- cmdArgs e2fcmds
    let header = L.concat [L8.pack "converted from ", L8.pack inputf]
    (L.readFile inputf) >>= (L.writeFile outputf . writeSeq 72 header . fromJust . readSeq . decompress)
    
