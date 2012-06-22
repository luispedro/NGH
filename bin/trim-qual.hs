{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Posix.Files
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.NGH.FastQ
import Data.NGH.Formats.FastQ
import Data.Conduit
import Data.Maybe
import Data.IORef
import Control.Monad
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB -- bytes
import Data.NGH.Trim

import Utils

data TrimCmd = TrimCmd
                { input :: String
                , output :: String
                , minQuality :: Int
                , minLength :: Int
                , filterAs :: Bool
                , maxNs :: Maybe Int
                , format :: String
                } deriving (Eq, Show, Data, Typeable)

trimcmds = TrimCmd
            { input = "-" &= argPos 0 &= typ "Input-file"
            , output = "-" &= argPos 1 &= typ "Output-file"
            , minQuality = 30 &= help "Minimal quality"
            , minLength = 18  &= help "Minimum read length"
            , filterAs = True &= help "Filter A-only sequences"
            , maxNs = Nothing &= help "Maximum number of Ns (default: ∞)"
            , format = "illumina" &= help "Format: `illumina` or `sanger`"
            } &=
            verbosity &=
            summary sumtext &=
            details ["Trim FastQ files based on qualities"]
    where sumtext = "trim-qual v0.1 (C) Luis Pedro Coelho 2012"



isgood :: Int -> Bool -> Maybe Int ->  DNAwQuality -> Bool
isgood mL fA mNs x = ((S.length $ dna_seq x) > mL)
                        &&
                    ((not fA) || (not $ allAs x))
                        &&
                    (maybe True (\m -> (S8.count 'N' $ dna_seq x) <= m) mNs)

allAs = isNothing . S8.findIndex (/= 'A') . dna_seq

main :: IO ()
main = do
    TrimCmd finput foutput mQ mL fAs mNs fmt <- cmdArgs trimcmds
    let parser = if fmt == "illumina"
                        then illumina
                        else phred
    total <- newIORef (0 :: Integer)
    good <- newIORef (0 :: Integer)
    fs <- fileSize `liftM` getFileStatus finput
    p <- progress fs (fromIntegral . S.length)
    _ <- runResourceT $
        CB.sourceFile finput
        =$= exec_action p
        =$= mayunzip finput
        =$= (fastQConduit parser)
        =$= counter total
        =$= CL.filter (isgood mL fAs mNs . trimLS (fromIntegral mQ))
        =$= counter good
        =$= CL.map (strict . fastQwrite)
        $$ CB.sinkFile foutput
    t <- readIORef total
    g <- readIORef good
    putStrLn $ concat ["Processed ", show t, " sequences."]
    putStrLn $ concat ["Kept ", show g, " (", take 4 $ show (100.0*fromIntegral g/fromIntegral t), "%) of them."]
