{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB -- bytes
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.NGH.FastQ

import Data.Conduit -- the core library
import Data.Conduit.Zlib (ungzip)
import Control.Monad
import Control.Monad.Trans
import System.Environment

main = do
    args <- getArgs
    case args of
        [fname] -> do
            t <- runResourceT $
                    CB.sourceFile fname
                    =$= ungzip
                    =$= CB.lines
                    $$ CL.fold (\a _ -> (a+1)) 0
            print (concat ["Nr seqs: ", show t])
        _ -> putStrLn "Usage: runghc Count.hs <FASTQ FILE>"

