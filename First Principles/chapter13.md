# Chapter 13 exercises

## Intermission: Check your understanding

```haskell
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Char8 as B
import qualified Data.Locator as DL
import qualified Data.Time.Clock.POSIX as PSX
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info as NI
import qualified Safe
import Control.Exception ( mask, try )
import Control.Monad ( forever, when )
import Data.Bits
import Data.Bits.Bitwise ( fromListBE )
import Data.List.Split ( chunksOf )
import Database.Blacktip.Types
import System.IO.Unsafe ( unsafePerformIO )
```

1. `forever` and `when`

2. `Data.Bits` and `Database.Blacktip.Types`

3. Blacktips data and type definitions

4.
    ```haskell
    writeTimestamp :: MV.MVar ServerState -> FPC.FilePath -> IO CC.ThreadId
    writeTimestamp s path = do
        CC.forkIO go
        where go = forever $ do
            ss <- MV.readMVar s
            mask $ \_ -> do
                FS.writeFile path
                (B.pack (show (ssTime ss)))
                -- sleep for 1 second
            CC.threadDelay 1000000
    ```
    a. `Control.Concurrent.MVar`, `Filesystem.Path.CurrentOS` and `Control.Concurrent`

    b. `import qualified Filesystem as FS`

    c. `import Control.Monad ( forever, when )`