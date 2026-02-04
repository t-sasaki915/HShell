{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component.Window.Property
    ( WindowProperty (..)
    , IsWindowProperty (..)
    , WindowTitle (..)
    , WindowIcon (..)
    , WindowCursor (..)
    , WindowSize (..)
    , WindowPosition (..)
    , WindowBrush (..)
    , WindowChildren (..)
    ) where

import           Control.Monad                   (forM_, void)
import           Data.Bits                       ((.|.))
import           Data.Data                       (Typeable, cast)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Foreign                         (intPtrToPtr)
import           Graphics.GUI
import           Graphics.GUI.Component          (GUIComponent (..),
                                                  IsGUIComponent (render))
import           Graphics.GUI.Component.Property (IsGUIComponentProperty)
import qualified Graphics.GUI.Foreign            as Win32
import qualified Graphics.Win32                  as Win32

data WindowProperty = forall a. (Typeable a, Eq a, IsWindowProperty a) => WindowProperty a

instance Eq WindowProperty where
    (WindowProperty a) == (WindowProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

class Eq a => IsWindowProperty a where
    applyProperty :: a -> Win32.HWND -> IO ()

instance IsWindowProperty WindowProperty where
    applyProperty (WindowProperty a) = applyProperty a

instance IsGUIComponentProperty WindowProperty

newtype WindowTitle    = WindowTitle    Text           deriving Eq
newtype WindowIcon     = WindowIcon     Icon           deriving Eq
newtype WindowCursor   = WindowCursor   Cursor         deriving Eq
newtype WindowSize     = WindowSize     (Int, Int)     deriving Eq
newtype WindowPosition = WindowPosition (Int, Int)     deriving Eq
newtype WindowBrush    = WindowBrush    Brush          deriving Eq
newtype WindowChildren = WindowChildren [GUIComponent] deriving Eq

instance IsGUIComponentProperty WindowTitle
instance IsGUIComponentProperty WindowIcon
instance IsGUIComponentProperty WindowCursor
instance IsGUIComponentProperty WindowSize
instance IsGUIComponentProperty WindowPosition
instance IsGUIComponentProperty WindowBrush
instance IsGUIComponentProperty WindowChildren

instance IsWindowProperty WindowTitle where
    applyProperty (WindowTitle title) windowHWND =
        Win32.setWindowText windowHWND (Text.unpack title)

instance IsWindowProperty WindowIcon where
    applyProperty (WindowIcon icon) windowHWND =
        toWin32Icon icon >>= \icon' ->
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HICON icon'

instance IsWindowProperty WindowCursor where
    applyProperty (WindowCursor cursor) windowHWND =
        Win32.loadCursor Nothing (toWin32Cursor cursor) >>= \cursor' ->
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HCURSOR cursor'

instance IsWindowProperty WindowSize where
    applyProperty (WindowSize (width, height)) windowHWND =
        void $
            Win32.c_SetWindowPos windowHWND
                Win32.nullPtr
                0
                0
                (fromIntegral width)
                (fromIntegral height)
                (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

instance IsWindowProperty WindowPosition where
    applyProperty (WindowPosition (x, y)) windowHWND =
        void $
            Win32.c_SetWindowPos
                windowHWND
                Win32.nullPtr
                (fromIntegral x)
                (fromIntegral y)
                0
                0
                (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

instance IsWindowProperty WindowBrush where
    applyProperty (WindowBrush brush) windowHWND = do
        Win32.c_GetClassLongPtr windowHWND Win32.gCLP_HBRBACKGROUND >>= \oldBrush ->
            void $ Win32.c_DeleteObject (intPtrToPtr $ fromIntegral oldBrush)

        toWin32Brush brush >>= \brush' -> do
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HBRBACKGROUND brush'

            Win32.withTString "WINDOW_BRUSH" $ \pName ->
                void $ Win32.c_SetProp windowHWND pName brush'

instance IsWindowProperty WindowChildren where
    applyProperty (WindowChildren children) windowHWND =
        forM_ children $ \(GUIComponent child) ->
            render child (Just windowHWND)
