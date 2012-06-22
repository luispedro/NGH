{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Posix.Files
import qualified Data.ByteString as S
import Data.NGH.FastQ
import Data.NGH.Formats.FastQ
import Data.Conduit
import Data.Maybe
import Control.Monad
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB -- bytes

import Utils hiding (progress)
import qualified Utils

data FastqCmd = FastqCmd
                { input :: String
                , output :: String
                , format :: String
                , oformat :: Maybe String
                , progress :: Bool
                } deriving (Eq, Show, Data, Typeable)

fastqcmds = FastqCmd
            { input = "-" &= argPos 0 &= typ "Input-file"
            , output = "-" &= argPos 1 &= typ "Output-file"
            , format = "illumina" &= help "Format: `illumina` or `sanger`"
            , oformat = Nothing &= help "Output format"
            , progress = False &= help "Show progress"
            } &=
            verbosity &=
            summary sumtext &=
            details ["FastQ manipulator"]
    where sumtext = "fastq v0.1 (C) Luis Pedro Coelho 2012"

main :: IO ()
main = do
    FastqCmd finput foutput ifmt ofmt use_progress <- cmdArgs fastqcmds
    let parser f = if f == "illumina"
                        then illumina
                        else phred
    fs <- fileSize `liftM` getFileStatus finput
    p <- Utils.progress fs (fromIntegral . S.length)
    _ <- runResourceT $
        CB.sourceFile finput
        =$= exec_action p
        =$= mayunzip finput
        =$= fastQConduit (parser ifmt)
        =$= CL.map (strict . fastQwrite (parser . fromMaybe "illumina" $ ofmt))
        $$ CB.sinkFile foutput
    return ()

