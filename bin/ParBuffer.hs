module ParBuffer
    ( parBuffer
    , parBufferN
    ) where
import Control.Parallel
import Control.Monad
import Data.Conduit
import Data.Conduit.Internal (pipePush, pipeClose, noInput)

parBuffer :: (Monad m) => Pipe i o m r -> Pipe i o m r
parBuffer p@Done{} = p
parBuffer p@Leftover{} = p
parBuffer (PipeM act f) = PipeM (parBuffer `liftM` act) f
parBuffer (NeedInput t f) = NeedInput (parBuffer . t) f
parBuffer p@(HaveOutput next f o) = NeedInput push (noInput p)
    where
        push i = nc `par` output
            where
                nc = pipePush i next
                output = HaveOutput (parBuffer nc) (pipeClose nc) o

parBufferN 0 = id
parBufferN n = (parBufferN (n-1)) . parBuffer
