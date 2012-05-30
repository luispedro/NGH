{-# LANGUAGE BangPatterns #-}
import System.Environment
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.NGH.FastQ
import Data.NGH.Formats.FastQ
import Data.Conduit
import Data.List (isSuffixOf)
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB -- bytes
import Data.NGH.Trim

main :: IO ()
main = do
    args <- getArgs
    case args of
        [finput, foutput] -> trimFile finput foutput
        _ -> putStrLn "trim-qual <FASTQ-FILE> <OUTPUT-NAME>"


idConduit = NeedInput pass (Done Nothing ())
    where
        pass = HaveOutput idConduit (return ())

mayunzip finput
    | "gz" `isSuffixOf` finput = ungzip
    | otherwise = idConduit

isgood :: DNAwQuality -> (Bool,DNAwQuality)
isgood x = ((S.length $ dna_seq x) > 25, x)

strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

trimFile finput foutput = do
    (_,(g,t)) <- runResourceT $
        CB.sourceFile finput
        =$= mayunzip finput
        =$= CB.lines
        =$= fastQConduit illumina
        =$= CL.map (isgood . trimLS 30)
        $$ (CL.filter fst
            =$= CL.map (strict . fastQwrite . snd)
            =$ CB.sinkFile foutput)
                    `CL.zipSinks`
            (CL.map fst
               =$ CL.fold (\(!g,!t) v -> (if v then (g+1) else g, t+1)) (0.0 :: Double,0.0 :: Double))
    putStrLn $ concat ["Processed ", show t, " sequences."]
    putStrLn $ concat ["Kept ", show g, " (", take 4 $ show (100.0*g/t), "%) of them."]
