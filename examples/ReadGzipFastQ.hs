module Main where

import qualified Data.Conduit.Binary as CB -- bytes
import Data.Conduit -- the core library
import Data.Conduit.Zlib (ungzip)
import Data.NGH.FastQ

main = do
    fastq <- runResourceT $
        CB.sourceFile "input.fq.gz"
        =$= ungzip
        $$ fastq_sink
    print `mapM_` fastq
    return ()
