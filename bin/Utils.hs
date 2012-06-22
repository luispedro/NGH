module Utils
    ( strict
    , exec_action
    , modifyIORef'
    , counter
    , progress
    , mayunzip
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Conduit.Internal
import Data.IORef
import Data.List (isSuffixOf)
import Control.Monad
import Control.Monad.Trans
import Data.Conduit.Zlib (ungzip)


strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

{-| exec_action executes an action for every element seen and passes it on -}
exec_action :: (Monad m) => (a -> m ()) -> Conduit a m a
exec_action act = await >>= maybe (return ()) (\s -> PipeM (act s >> return (yield s >> exec_action act)))

{-| strict version of modifyIORef -}
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

{-| A counter will increment the IORef ref every time it sees an input -}
counter :: (Monad (t IO), Num a1, MonadTrans t) => IORef a1 -> Conduit a (t IO) a
counter ref = exec_action . const . lift $ modifyIORef' ref (+1)

progress ::  (Integral a, MonadIO (t IO), MonadTrans t) => a -> (t1 -> Integer) -> IO (t1 -> t IO ())
progress totalSize sizef = do
        partialref <- newIORef (0 :: Integer)
        return $ \s -> do
            v0 <- lift (readIORef partialref)
            lift $ modifyIORef' partialref (+ sizef s)
            v1 <- lift (readIORef partialref)
            when (roundP v0 /= roundP v1)
                (liftIO $ putStrLn $ concat ["Finished ", show . roundP $ v1, "%"])
    where
        roundP :: Integer -> Integer
        roundP v = 5 * (round $ fromIntegral v / fromIntegral totalSize * (20.0 :: Double))

transformif :: Monad m => Bool -> Pipe l a a r m r -> Pipe l a a r m r
transformif cond trans
    | cond = trans
    | otherwise = idP

mayunzip :: (Monad m, MonadUnsafeIO m, MonadThrow m) => String -> Conduit S.ByteString m S.ByteString
mayunzip finput = transformif ("gz" `isSuffixOf` finput) ungzip


