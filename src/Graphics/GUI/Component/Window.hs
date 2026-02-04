module Graphics.GUI.Component.Window (Window (..)) where

import           Control.Monad                          (void, when)
import           Data.IORef                             (atomicModifyIORef',
                                                         readIORef)
import qualified Data.Map                               as Map
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Foreign                                (freeHaskellFunPtr,
                                                         intPtrToPtr)
import qualified Framework.TEA.Internal                 as TEAInternal
import           Graphics.GUI                           (WindowStyle,
                                                         toWin32WindowStyle)
import           Graphics.GUI.Component                 (IsGUIComponent (..))
import           Graphics.GUI.Component.Window.Property (IsWindowProperty (..),
                                                         WindowProperty (..))
import qualified Graphics.GUI.Foreign                   as Win32
import qualified Graphics.GUI.Internal                  as Internal
import qualified Graphics.Win32                         as Win32
import qualified System.Win32                           as Win32

data Window = Window Text WindowStyle [WindowProperty] deriving Eq

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
                    (toWin32WindowStyle windowStyle)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    parentHWND
                    Nothing
                    mainInstance
                    typicalWindowProc

        mapM_ (`applyProperty` window) windowProperties

        _ <- Win32.showWindow window Win32.sW_SHOWNORMAL
        Win32.updateWindow window

        void $ atomicModifyIORef' Internal.activeWindowCountRef $ \n -> (n + 1, n + 1)

        pure window

typicalWindowProc :: Win32.LPPAINTSTRUCT -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
typicalWindowProc hwnd wMsg wParam lParam
    | wMsg == Win32.wM_DESTROY = do
        remainingWindow <- atomicModifyIORef' Internal.activeWindowCountRef $ \n -> (n - 1, n - 1)

        when (remainingWindow <= 0) $
            Win32.postQuitMessage 0

        pure 0

    | wMsg == Win32.wM_COMMAND = do
        let notification = Win32.hIWORD (fromIntegral wParam)
            targetHWND = intPtrToPtr (fromIntegral lParam)

        case notification of
            0 -> do -- BN_CLICKED
                buttonClickEventHandlers <- readIORef TEAInternal.buttonClickEventHandlersRef

                case Map.lookup targetHWND buttonClickEventHandlers of
                    Just action -> TEAInternal.performUpdate action >> pure 0
                    Nothing     -> Win32.defWindowProc (Just hwnd) wMsg wParam lParam

            _ ->
                Win32.defWindowProc (Just hwnd) wMsg wParam lParam

    | wMsg == Win32.wM_NCDESTROY =
        cleanupGDIs hwnd >>
            pure 0

    | otherwise =
        Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

-- TODO: cleanupEventHandlers

cleanupGDIs :: Win32.HWND -> IO ()
cleanupGDIs hwnd = do
    let callback _ _ hData _ =
            void (Win32.c_DeleteObject hData) >>
                pure True

    callbackPtr <- Win32.makePropEnumProcEx callback

    void $ Win32.c_EnumPropsEx hwnd callbackPtr 0

    freeHaskellFunPtr callbackPtr
