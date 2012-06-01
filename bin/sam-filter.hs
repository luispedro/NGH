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
import ParBuffer


data SamCmd = SamCmd
                { input :: String
                , output :: String
                } deriving (Eq, Show, Data, Typeable)

samcmds = SamCmd
            { input = "-" &= argPos 0
            , output = "-" &= argPos 1
            } &=
            verbosity &=
            summary sumtext &=
            details ["Filter non-match queries from SAM files"]
    where sumtext = "sam-filter v0.1 (C) Luis Pedro Coelho 2012"




-- mayunzip :: (Monad m, MonadUnsafeIO m, MonadThrow m) => String -> Pipe Void S.ByteString m () -> Pipe Void S.ByteString m ()
mayunzip finput c
    | "gz" `isSuffixOf` finput = (c $= parBufferN 32 ungzip)
    | otherwise = c

strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

main :: IO ()
main = do
    SamCmd finput foutput <- cmdArgs samcmds
    (_,(g,t)) <- runResourceT $
        (mayunzip finput $ CB.sourceFile finput)
        =$= CB.lines
        =$= CL.filter ((/='@').S8.head)
        =$= CL.map (\ell -> (isAligned $ readSamLine $ L.fromChunks [ell], ell))
        $$ (CL.filter fst
            =$= CL.map (\(_,s) -> S.concat [s,S8.pack "\n"])
            =$ CB.sinkFile foutput)
                    `CL.zipSinks`
            (CL.map fst
               =$ CL.fold (\(!g,!t) v -> (if v then (g+1) else g, t+1)) (0.0 :: Double,0.0 :: Double))
    putStrLn $ concat ["Processed ", show t, " lines."]
    putStrLn $ concat ["There were matches in ", show $ round g, " (", take 4 $ show (100.0*g/t), "%) of them."]
