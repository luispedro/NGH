{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
import System.Console.CmdArgs
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.NGH.Alignments
import Data.NGH.Formats.Sam
import Data.Void
import Data.Conduit
import Data.Maybe
import Data.List (isSuffixOf)
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB -- bytes
import Data.NGH.Trim
import Control.Monad
import Data.IORef

import Utils

data SamCmd = SamCmd
                { input :: String
                , output :: String
                } deriving (Eq, Show, Data, Typeable)

samcmds = SamCmd
            { input = "-" &= argPos 0 &= typ "Input-file"
            , output = "-" &= argPos 1 &= typ "Output-file"
            } &=
            verbosity &=
            summary sumtext &=
            details ["Filter non-match queries from SAM files"]
    where sumtext = "sam-filter v0.1 (C) Luis Pedro Coelho 2012"



main :: IO ()
main = do
    SamCmd finput foutput <- cmdArgs samcmds
    v <- getVerbosity
    let q = v == Quiet
    total <- newIORef (0.0 :: Double)
    good <- newIORef (0.0 :: Double)
    _ <- runResourceT $
        readerC finput
        =$= mayunzip finput
        =$= CB.lines
        =$= CL.filter ((/='@').S8.head)
        =$= counter total
        =$= CL.filter (\ell -> (isAligned $ readSamLine $ L.fromChunks [ell]))
        =$= counter good
        =$= CL.map (\s -> S.concat [s,S8.pack "\n"])
        $$ writerC foutput
    t <- readIORef total
    g <- readIORef good
    unless q
        (putStrLn $ concat ["Processed ", show $ round t, " lines."])
    unless q
        (putStrLn $ concat ["There were matches in ", show $ round g, " (", take 4 $ show (100.0*g/t), "%) of them."])
