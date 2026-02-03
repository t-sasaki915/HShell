module Graphics.GUI.Internal (withRandomTString) where

import qualified Data.UUID      as UUID
import qualified Data.UUID.V4   as UUID
import qualified Graphics.Win32 as Win32

withRandomTString :: (Win32.LPTSTR -> IO a) -> IO a
withRandomTString f =
    UUID.nextRandom >>= \uuid ->
        Win32.withTString (UUID.toString uuid) f
