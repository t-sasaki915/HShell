{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component.Button.Property
    ( ButtonProperty (..)
    , IsButtonProperty (..)
    , ButtonLabel (..)
    , ButtonSize (..)
    , ButtonPosition (..)
    , ButtonClicked (..)
    ) where

import           Control.Monad                   (void)
import           Data.Bits                       ((.|.))
import           Data.Data                       (Typeable, cast)
import           Data.IORef                      (atomicModifyIORef')
import qualified Data.Map                        as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Framework.TEA                   (IsMsg)
import qualified Framework.TEA.Internal          as TEAInternal
import           Graphics.GUI.Component.Property (IsGUIComponentProperty)
import qualified Graphics.GUI.Foreign            as Win32
import qualified Graphics.Win32                  as Win32

data ButtonProperty = forall a. (Typeable a, IsButtonProperty a) => ButtonProperty a

instance Eq ButtonProperty where
    (ButtonProperty a) == (ButtonProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

class Eq a => IsButtonProperty a where
    applyProperty :: a -> Win32.HWND -> IO ()

instance IsButtonProperty ButtonProperty where
    applyProperty (ButtonProperty a) = applyProperty a

instance IsGUIComponentProperty ButtonProperty

newtype ButtonLabel    = ButtonLabel    Text       deriving Eq
newtype ButtonSize     = ButtonSize     (Int, Int) deriving Eq
newtype ButtonPosition = ButtonPosition (Int, Int) deriving Eq
data    ButtonClicked  = forall a. (Typeable a, IsMsg a) => ButtonClicked a

instance Eq ButtonClicked where
    (ButtonClicked a) == (ButtonClicked b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance IsGUIComponentProperty ButtonLabel
instance IsGUIComponentProperty ButtonSize
instance IsGUIComponentProperty ButtonPosition
instance IsGUIComponentProperty ButtonClicked

instance IsButtonProperty ButtonLabel where
    applyProperty (ButtonLabel label) buttonHWND =
        Win32.setWindowText buttonHWND (Text.unpack label)

instance IsButtonProperty ButtonSize where
    applyProperty (ButtonSize (width, height)) buttonHWND =
        void $
            Win32.c_SetWindowPos buttonHWND
                Win32.nullPtr
                0
                0
                (fromIntegral width)
                (fromIntegral height)
                (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

instance IsButtonProperty ButtonPosition where
    applyProperty (ButtonPosition (x, y)) buttonHWND =
        void $
            Win32.c_SetWindowPos
                buttonHWND
                Win32.nullPtr
                (fromIntegral x)
                (fromIntegral y)
                0
                0
                (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

instance IsButtonProperty ButtonClicked where
    applyProperty (ButtonClicked msg) buttonHWND =
        void $ atomicModifyIORef' TEAInternal.buttonClickEventHandlersRef $ \a ->
            let updatedMap = Map.insert buttonHWND (TEAInternal.Msg msg) a in
                (updatedMap, updatedMap)

