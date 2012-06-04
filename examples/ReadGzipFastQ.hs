module Main where

import qualified Data.Conduit.Binary as CB -- bytes
import Data.Conduit -- the core library
import Data.Conduit.Zlib (ungzip)
import Data.NGH.FastQ
import Data.NGH.Formats.FastQ
import Control.Monad.Trans

main = runResourceT $
        CB.sourceFile "input.fq.gz"
        =$= ungzip
        =$= fastQConduit illumina
        $$ sinkIO (return ())
                (\s -> return ())
                (\_ sq -> (liftIO (print sq)) >> (return IOProcessing))
                (\s -> return ())
