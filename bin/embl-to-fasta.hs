{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.NGH.Formats.Fasta
import Data.NGH.Formats.Embl
import Codec.Compression.GZip
import Utils

data E2FCmd = E2FCmd
        { input :: String
        , output :: String
        } deriving (Eq, Show, Data, Typeable)

e2fcmds = E2FCmd
        { input = "-" &= argPos 0 &= typ "Input-file"
        , output = "-" &= argPos 1 &= typ "Output-file"
        } &=
        verbosity &=
        summary sumtext &=
        details ["Convert Embl to Fasta files"]
    where sumtext = "embl-to-fasta v0.1 (C) Luis Pedro Coelho 2012"

main :: IO ()
main = do
    E2FCmd inputf outputf <- cmdArgs e2fcmds
    let header = L8.pack inputf
    (reader inputf) >>= (writer outputf . writeSeq 72 header . fromJust . readSeq . decompress)

    
