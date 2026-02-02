module Graphics.GUI.Component.Window (Window (..)) where

import           Data.Bits                              ((.|.))
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Graphics.GUI                           (WindowStyle,
                                                         toWin32WindowStyle)
import           Graphics.GUI.Component                 (IsGUIComponent (..))
import           Graphics.GUI.Component.Window.Property (IsWindowProperty (..),
                                                         WindowProperty (..))
import qualified Graphics.Win32                         as Win32
import qualified System.Win32                           as Win32

data Window = Window Text [WindowStyle] [WindowProperty] deriving Eq

instance IsGUIComponent Window where
    render (Window windowClassName windowStyle windowProperties) parentHWND = do
        mainInstance <- Win32.getModuleHandle Nothing

        let windowClass = Win32.mkClassName (Text.unpack windowClassName)

        _ <- Win32.registerClass
                ( Win32.cS_VREDRAW + Win32.cS_HREDRAW
                , mainInstance
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , windowClass
                )

        window <- Win32.createWindow
                    windowClass
                    ""
                    (foldl (.|.) 0 (map toWin32WindowStyle windowStyle))
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    parentHWND
                    Nothing
                    mainInstance
                    typicalWindowProc

        mapM_ (flip applyProperty window) windowProperties

        _ <- Win32.showWindow window Win32.sW_SHOWNORMAL
        Win32.updateWindow window

        pure window

typicalWindowProc :: Win32.LPPAINTSTRUCT -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
typicalWindowProc hwnd wmsg wParam lParam
    | wmsg == Win32.wM_DESTROY = Win32.sendMessage hwnd Win32.wM_QUIT 1 0 >> pure 0
    | otherwise          = Win32.defWindowProcSafe (Just hwnd) wmsg wParam lParam
