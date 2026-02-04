module Graphics.GUI.Internal
    ( activeWindowCountRef
    , withRandomTString
    ) where

import           Data.IORef       (IORef, newIORef)
import qualified Data.UUID        as UUID
import qualified Data.UUID.V4     as UUID
import qualified Graphics.Win32   as Win32
import           System.IO.Unsafe (unsafePerformIO)

activeWindowCountRef :: IORef Int
activeWindowCountRef = unsafePerformIO (newIORef 0)
{-# NOINLINE activeWindowCountRef #-}

withRandomTString :: (Win32.LPTSTR -> IO a) -> IO a
withRandomTString f =
    UUID.nextRandom >>= \uuid ->
        Win32.withTString (UUID.toString uuid) f
