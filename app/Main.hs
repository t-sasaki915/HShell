{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-record-selectors #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (when)
import           Data.Coerce                (Coercible, coerce)
import           Data.Functor               ((<&>))
import qualified Data.List                  as List
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Graphics.Win32             hiding (try)
import           System.Process             (callCommand)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.DLL           (getModuleHandle)
import           System.Win32.Info.Computer (getSystemMetrics)

data Win32WindowProperty = Win32WindowIcon             { nativeIcon :: Icon }
                         | Win32WindowCursor           { nativeCursor :: Cursor }
                         | Win32WindowSize             { width :: Int, height :: Int }
                         | Win32WindowInitialPosition  { x :: Int, y :: Int }
                         | Win32WindowBrush            { nativeBrush :: HBRUSH }

data Win32WindowStyle = Win32WindowStylePopup

fromWin32WindowStyle :: Win32WindowStyle -> WindowStyle
fromWin32WindowStyle Win32WindowStylePopup = wS_POPUP

data Win32GUIComponent = Win32Window Text Text Win32WindowStyle [Win32WindowProperty] [Win32GUIComponent]

main :: IO ()
main = do
    callCommand "wpeinit"

    displayWidth  <- getSystemMetrics sM_CXSCREEN
    displayHeight <- getSystemMetrics sM_CYSCREEN
    brush         <- createSolidBrush (rgb 255 255 255)

    let testWindow =
            Win32Window
                "HShell"
                "HShell"
                Win32WindowStylePopup
                [ Win32WindowIcon iDI_QUESTION
                , Win32WindowCursor iDC_WAIT
                , Win32WindowSize displayWidth displayHeight
                , Win32WindowInitialPosition 0 0
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

renderComponent :: Win32GUIComponent -> IO HWND
renderComponent (Win32Window windowClass windowTitle windowStyle windowProperties windowChildren) = do
    mainInstance <- getModuleHandle Nothing

    let
        windowClass'  = mkClassName (Text.unpack windowClass)
        windowIcon    = nativeIcon   <$> List.find (\(Win32WindowIcon _)              -> True) windowProperties
        windowCursor  = nativeCursor <$> List.find (\(Win32WindowCursor _)            -> True) windowProperties
        windowBrush   = nativeBrush  <$> List.find (\(Win32WindowBrush _)             -> True) windowProperties
        windowSize    = (\a -> (width a, height )) <$> List.find (\(Win32WindowSize _ _)            -> True) windowProperties
        windowInitPos = List.find (\(Win32WindowInitialPosition _ _) -> True) windowProperties

    _ <- registerClass
            ( cS_VREDRAW + cS_HREDRAW
            , mainInstance
            , windowIcon <&> coerce
            , windowCursor <&> (\(Win32WindowCursor x) -> x)
            , windowBrush <&> (\(Win32WindowBrush x) -> x)
            , Nothing
            , windowClass'
            )

    window <- createWindow
                windowClass'
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
