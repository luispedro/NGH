{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Posix.Files
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.NGH.FastQ
import Data.NGH.Formats.FastQ
import Data.Conduit
import Data.Conduit.Internal
import Data.Maybe
import Data.IORef
import Data.List (isSuffixOf)
import Control.Monad
import Control.Monad.Trans
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB -- bytes
import Data.NGH.Trim


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



exec_action :: (Monad m) => (a -> m ()) -> Conduit a m a
exec_action act = await >>= maybe (return ()) (\s -> PipeM (act s >> return (yield s >> exec_action act)))


modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

counter ref = exec_action . const . lift $ modifyIORef' ref (+1)

progress totalSize sizef = do
        partialref <- newIORef (0 :: Integer)
        return $ \s -> do
            v0 <- lift (readIORef partialref)
            lift $ modifyIORef' partialref (+ sizef s)
            v1 <- lift (readIORef partialref)
            when (roundP v0 /= roundP v1)
                (liftIO $ putStrLn $ concat ["Finished ", show . roundP $ v1, "%"])
    where
        roundP v = 5 * (round $ fromIntegral v / fromIntegral totalSize * 20.0)

transformif cond trans
    | cond = trans
    | otherwise = idP

mayunzip :: (Monad m, MonadUnsafeIO m, MonadThrow m) => String -> Conduit S.ByteString m S.ByteString
mayunzip finput = transformif ("gz" `isSuffixOf` finput) ungzip

isgood :: Int -> Bool -> Maybe Int ->  DNAwQuality -> Bool
isgood mL fA mNs x = ((S.length $ dna_seq x) > mL)
                        &&
                    ((not fA) || (not $ allAs x))
                        &&
                    (maybe True (\m -> (S8.count 'N' $ dna_seq x) <= m) mNs)

allAs = isNothing . S8.findIndex (/= 'A') . dna_seq

strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

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
