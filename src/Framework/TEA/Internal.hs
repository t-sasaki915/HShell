{-# LANGUAGE ExistentialQuantification #-}

module Framework.TEA.Internal
    ( IsModel
    , IsMsg
    , Model (..)
    , Msg (..)
    , modelRef
    , buttonClickEventHandlersRef
    , updateFuncRef
    , performUpdate
    ) where

import           Data.Data      (Typeable)
import           Data.IORef     (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Map       (Map)
import           GHC.IO         (unsafePerformIO)
import qualified Graphics.Win32 as Win32

class IsModel a
class Eq a => IsMsg a

data Model = forall a. (Typeable a, IsModel a) => Model a | NoModel
data Msg = forall a. (Typeable a, IsMsg a) => Msg a

modelRef :: IORef Model
modelRef = unsafePerformIO (newIORef NoModel)
{-# NOINLINE modelRef #-}

updateFuncRef :: IORef (Msg -> Model -> IO Model)
updateFuncRef = unsafePerformIO (newIORef (const $ const $ pure NoModel))
{-# NOINLINE updateFuncRef #-}

buttonClickEventHandlersRef :: IORef (Map Win32.HWND Msg)
buttonClickEventHandlersRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE buttonClickEventHandlersRef #-}

performUpdate :: Msg -> IO ()
performUpdate msg = do
    updateFunc   <- readIORef updateFuncRef
    currentModel <- readIORef modelRef

    newModel <- updateFunc msg currentModel
    _        <- atomicModifyIORef' modelRef (const (newModel, newModel))

    pure ()
