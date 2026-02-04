{-# LANGUAGE ExistentialQuantification #-}

module Framework.TEA.Internal
    ( IsModel
    , IsMsg
    , Model (..)
    , Msg (..)
    , modelRef
    , buttonClickEventHandlersRef
    ) where

import           Data.IORef     (IORef, newIORef)
import           Data.Map       (Map)
import           GHC.IO         (unsafePerformIO)
import qualified Graphics.Win32 as Win32

class IsModel a
class Eq a => IsMsg a

data Model = forall a. IsModel a => Model a | NoModel
data Msg = forall a. IsMsg a => Msg a

modelRef :: IORef Model
modelRef = unsafePerformIO (newIORef NoModel)
{-# NOINLINE modelRef #-}

buttonClickEventHandlersRef :: IORef (Map Win32.HWND Msg)
buttonClickEventHandlersRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE buttonClickEventHandlersRef #-}
