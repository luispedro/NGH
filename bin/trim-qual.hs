{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Environment
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.NGH.FastQ
import Data.NGH.Formats.FastQ
import Data.Void
import Data.Conduit
import Data.Maybe
import Data.List (isSuffixOf)
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util as CU
import qualified Data.Conduit.Binary as CB -- bytes
import Data.NGH.Trim


data TrimCmd = TrimCmd
                { input :: String
                , output :: String
                , minLength :: Int
                , filterAs :: Bool
                , maxNs :: Int
                } deriving (Eq, Show, Data, Typeable)

trimcmds = TrimCmd
            { input = "-" &= argPos 0 -- &= help "Input file"
            , output = "-" &= argPos 1 -- &= help "Output file"
            , minLength = 18  &= help "Minimum read length"
            , filterAs = True &= help "Filter A-only sequences"
            , maxNs = -1 &= help "Maximum number of Ns (default: -1, interpreted as ∞)"
            } &=
            verbosity &=
            summary sumtext &=
            details ["Trim FastQ files based on qualities"]
    where sumtext = "trim-qual v0.1 (C) Luis Pedro Coelho 2012"




-- mayunzip :: (Monad m, MonadUnsafeIO m, MonadThrow m) => String -> Pipe Void S.ByteString m () -> Pipe Void S.ByteString m ()
mayunzip finput c
    | "gz" `isSuffixOf` finput = (c $= ungzip)
    | otherwise = c

isgood :: Int -> Bool -> Int ->  DNAwQuality -> (Bool,DNAwQuality)
isgood mL fA mNs x = (isgood' x, x)
    where
        isgood' x = ((S.length $ dna_seq x) > mL)
                        &&
                    ((not fA) || (not $ allAs x))
                        &&
                    ((mNs < 0) || (S8.count 'N' $ dna_seq x) <= mNs)

allAs = isNothing . S8.findIndex (/= 'A') . dna_seq

strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

main :: IO ()
main = do
    TrimCmd finput foutput mL fAs mNs <- cmdArgs trimcmds
    (_,(g,t)) <- runResourceT $
        (mayunzip finput $ CB.sourceFile finput)
        =$= (fastQConduit illumina)
        =$= CL.map (isgood mL fAs mNs . trimLS 30)
        $$ (CL.filter fst
            =$= CL.map (strict . fastQwrite . snd)
            =$ CB.sinkFile foutput)
                    `CU.zipSinks`
            (CL.map fst
               =$ CL.fold (\(!g,!t) v -> (if v then (g+1) else g, t+1)) (0.0 :: Double,0.0 :: Double))
    putStrLn $ concat ["Processed ", show t, " sequences."]
    putStrLn $ concat ["Kept ", show g, " (", take 4 $ show (100.0*g/t), "%) of them."]
