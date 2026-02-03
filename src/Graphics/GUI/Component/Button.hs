module Graphics.GUI.Component.Button (Button (..)) where

import           Data.Bits                              ((.|.))
import           Data.Maybe                             (fromJust)
import           Foreign                                (intPtrToPtr)
import           Graphics.GUI.Component                 (IsGUIComponent (render))
import           Graphics.GUI.Component.Button.Property (ButtonProperty,
                                                         IsButtonProperty (..))
import qualified Graphics.Win32                         as Win32

newtype Button = Button [ButtonProperty] deriving Eq

instance IsGUIComponent Button where
    render (Button buttonProperties) parentHWND = do
        parentInstance <- Win32.c_GetWindowLongPtr (fromJust parentHWND) (-6)

        button <- Win32.createWindow
            (Win32.mkClassName "BUTTON")
            ""
            (Win32.wS_TABSTOP .|. Win32.wS_VISIBLE .|. Win32.wS_CHILD .|. Win32.bS_DEFPUSHBUTTON)
            Nothing
            Nothing
            Nothing
            Nothing
            parentHWND
            Nothing
            (intPtrToPtr $ fromIntegral parentInstance)
            (const $ const $ const $ const $ pure 0)

        mapM_ (`applyProperty` button) buttonProperties

        pure button
