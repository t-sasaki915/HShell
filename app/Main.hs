{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (when)
import           Data.Functor               ((<&>))
import           Data.List.Extra            (firstJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Graphics.Win32             hiding (try)
import           System.Process             (callCommand)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.DLL           (getModuleHandle)
import           System.Win32.Info.Computer (getSystemMetrics)

data Win32WindowProperty = Win32WindowIcon             Icon
                         | Win32WindowCursor           Cursor
                         | Win32WindowSize             (Int, Int)
                         | Win32WindowInitialPosition  (Int, Int)
                         | Win32WindowBrush            HBRUSH

data Win32WindowStyle = Win32WindowStylePopup

fromWin32WindowStyle :: Win32WindowStyle -> WindowStyle
fromWin32WindowStyle Win32WindowStylePopup = wS_POPUP

class IsWin32GUIComponent a where
    render :: a -> IO HWND

data AnyWin32GUIComponent = forall a. IsWin32GUIComponent a => AnyWin32GUIComponent a

data Win32Window = Win32Window Text Text Win32WindowStyle [Win32WindowProperty] [AnyWin32GUIComponent]

instance IsWin32GUIComponent Win32Window where
    render (Win32Window windowClassName windowTitle windowStyle windowProperties windowChildren) = do
        mainInstance <- getModuleHandle Nothing

        let
            windowClass   = mkClassName (Text.unpack windowClassName)
            windowIcon    = flip firstJust windowProperties $ \case
                (Win32WindowIcon x)            -> Just x
                _                              -> Nothing
            windowCursor  = flip firstJust windowProperties $ \case
                (Win32WindowCursor x)          -> Just x
                _                              -> Nothing
            windowBrush   = flip firstJust windowProperties $ \case
                (Win32WindowBrush x)           -> Just x
                _                              -> Nothing
            windowSize    = flip firstJust windowProperties $ \case
                (Win32WindowSize x)            -> Just x
                _                              -> Nothing
            windowInitPos = flip firstJust windowProperties $ \case
                (Win32WindowInitialPosition x) -> Just x
                _                              -> Nothing

        windowIcon'   <- traverse (loadIcon Nothing) windowIcon
        windowCursor' <- traverse (loadCursor Nothing) windowCursor

        _ <- registerClass
                ( cS_VREDRAW + cS_HREDRAW
                , mainInstance
                , windowIcon'
                , windowCursor'
                , windowBrush
                , Nothing
                , windowClass
                )

        window <- createWindow
                    windowClass
                    (Text.unpack windowTitle)
                    (fromWin32WindowStyle windowStyle)
                    (windowInitPos <&> fst)
                    (windowInitPos <&> snd)
                    (windowSize <&> fst)
                    (windowSize <&> snd)
                    Nothing
                    Nothing
                    mainInstance
                    typicalWindowProc

        pure window

win32Window :: Text -> Text -> Win32WindowStyle -> [Win32WindowProperty] -> [AnyWin32GUIComponent] -> AnyWin32GUIComponent
win32Window a b c d e = AnyWin32GUIComponent (Win32Window a b c d e)

main :: IO ()
main = do
    callCommand "wpeinit"

    displayWidth  <- getSystemMetrics sM_CXSCREEN
    displayHeight <- getSystemMetrics sM_CYSCREEN
    brush         <- createSolidBrush (rgb 255 255 255)

    let testWindow =
            win32Window
                "HShell"
                "HShell"
                Win32WindowStylePopup
                [ Win32WindowIcon iDI_QUESTION
                , Win32WindowCursor iDC_WAIT
                , Win32WindowSize (displayWidth, displayHeight)
                , Win32WindowInitialPosition (0, 0)
                , Win32WindowBrush brush
                ]
                []

    hwnd <- createMainWindow displayWidth displayHeight typicalWindowProc
    messagePump hwnd

typicalWindowProc :: LPPAINTSTRUCT -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
typicalWindowProc hwnd wmsg wParam lParam
    | wmsg == wM_DESTROY = sendMessage hwnd wM_QUIT 1 0 >> pure 0
    | otherwise          = defWindowProcSafe (Just hwnd) wmsg wParam lParam

createMainWindow :: Int -> Int -> WindowClosure -> IO HWND
createMainWindow width height wndProc' = do
  let winClass = mkClassName "Hello"
  icon         <- loadIcon   Nothing iDI_QUESTION
  cursor       <- loadCursor Nothing iDC_WAIT
  bgBrush      <- createSolidBrush (rgb 255 255 255)
  mainInstance <- getModuleHandle Nothing
  _ <- registerClass
          ( cS_VREDRAW + cS_HREDRAW
          , mainInstance
          , Just icon
          , Just cursor
          , Just bgBrush
          , Nothing
          , winClass
          )
  w <- createWindow
                 winClass
                 "Hello, World example"
                 wS_POPUP
                 (Just 0)
                 (Just 0)
                 (Just width)
                 (Just height)
                 Nothing      -- no parent, i.e, root window is the parent.
                 Nothing      -- no menu handle
                 mainInstance
                 wndProc'
  _ <- showWindow w sW_SHOWNORMAL
  updateWindow w
  pure w

messagePump :: HWND -> IO ()
messagePump hwnd =
    allocaMessage $ \msg ->
        let pump =
                (try $ getMessage msg (Just hwnd) :: IO (Either SomeException Bool)) >>= \r ->
                    when (either (const False) id r) $
                        () <$ translateMessage msg >>
                            () <$ dispatchMessage msg >>
                                pump
        in pump
