module Graphics.GUI.Internal
    ( activeWindowCountRef
    ) where

import           Data.IORef       (IORef, newIORef)
import           System.IO.Unsafe (unsafePerformIO)

activeWindowCountRef :: IORef Int
activeWindowCountRef = unsafePerformIO (newIORef 0)
{-# NOINLINE activeWindowCountRef #-}
