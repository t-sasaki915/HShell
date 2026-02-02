{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -Wno-unsupported-calling-conventions #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (forM_, void, when)
import           Control.Monad.Writer.Lazy  (MonadWriter (tell), Writer,
                                             runWriter)
import           Data.Bits                  ((.|.))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Foreign                    (Int32, Ptr, intPtrToPtr)
import           Graphics.Win32             hiding (try)
import           System.Process             (callCommand)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.DLL           (getModuleHandle)
import           System.Win32.Info.Computer (getSystemMetrics)

foreign import stdcall "windows.h SetClassLongPtrW"
  c_SetClassLongPtr :: HWND -> Int32 -> Ptr () -> IO (Ptr ())

foreign import stdcall "windows.h SetWindowPos"
  c_SetWindowPos :: HWND -> HWND -> Int32 -> Int32 -> Int32 -> Int32 -> UINT -> IO BOOL

data Win32WindowProperty = forall a. IsWin32WindowProperty a => Win32WindowProperty a

class IsWin32GUIComponentProperty a

class IsWin32WindowProperty a where
    applyProperty :: a -> HWND -> IO ()

instance IsWin32WindowProperty Win32WindowProperty where
    applyProperty (Win32WindowProperty a) = applyProperty a

newtype Win32WindowTitle    = Win32WindowTitle Text
newtype Win32WindowIcon     = Win32WindowIcon Win32Icon
newtype Win32WindowCursor   = Win32WindowCursor Win32Cursor
newtype Win32WindowSize     = Win32WindowSize (Int, Int)
newtype Win32WindowPosition = Win32WindowPosition (Int, Int)
newtype Win32WindowBrush    = Win32WindowBrush HBRUSH
newtype Win32WindowChildren = Win32WindowChildren [Win32GUIComponent]

instance IsWin32GUIComponentProperty Win32WindowTitle
instance IsWin32GUIComponentProperty Win32WindowIcon
instance IsWin32GUIComponentProperty Win32WindowCursor
instance IsWin32GUIComponentProperty Win32WindowSize
instance IsWin32GUIComponentProperty Win32WindowPosition
instance IsWin32GUIComponentProperty Win32WindowBrush
instance IsWin32GUIComponentProperty Win32WindowChildren

instance IsWin32WindowProperty Win32WindowTitle where
    applyProperty (Win32WindowTitle title) windowHWND =
        setWindowText windowHWND (Text.unpack title)

instance IsWin32WindowProperty Win32WindowIcon where
    applyProperty (Win32WindowIcon icon) windowHWND =
        loadIcon Nothing (fromWin32Icon icon) >>= \icon' ->
            void $ c_SetClassLongPtr windowHWND (-14) icon'

instance IsWin32WindowProperty Win32WindowCursor where
    applyProperty (Win32WindowCursor cursor) windowHWND =
        loadCursor Nothing (fromWin32Cursor cursor) >>= \cursor' ->
            void $ c_SetClassLongPtr windowHWND (-12) cursor'

instance IsWin32WindowProperty Win32WindowSize where
    applyProperty (Win32WindowSize (width, height)) windowHWND =
        void $ c_SetWindowPos windowHWND (intPtrToPtr 0) 0 0 (fromIntegral width) (fromIntegral height) (sWP_NOMOVE .|. sWP_NOZORDER .|. sWP_NOACTIVATE)

instance IsWin32WindowProperty Win32WindowPosition where
    applyProperty (Win32WindowPosition (x, y)) windowHWND =
        void $ c_SetWindowPos windowHWND (intPtrToPtr 0) (fromIntegral x) (fromIntegral y) 0 0 (sWP_NOSIZE .|. sWP_NOZORDER .|. sWP_NOACTIVATE)

instance IsWin32WindowProperty Win32WindowBrush where
    applyProperty (Win32WindowBrush brush) windowHWND =
        void $ c_SetClassLongPtr windowHWND (-10) brush

instance IsWin32WindowProperty Win32WindowChildren where
    applyProperty (Win32WindowChildren children) window =
        forM_ children $ \(Win32GUIComponent child) ->
            render child (Just window)

data Win32WindowStyle = Win32WindowStylePopup
                      | Win32WindowStyleOverlapped
                      | Win32WindowStyleChild
                      deriving Eq

fromWin32WindowStyle :: Win32WindowStyle -> WindowStyle
fromWin32WindowStyle Win32WindowStylePopup      = wS_POPUP
fromWin32WindowStyle Win32WindowStyleOverlapped = wS_OVERLAPPEDWINDOW
fromWin32WindowStyle Win32WindowStyleChild      = wS_CHILD

data Win32Icon = Win32IconApplication
               | Win32IconHand
               | Win32IconQuestion
               | Win32IconExclamation
               | Win32IconAsterisk
               deriving Eq

fromWin32Icon :: Win32Icon -> Icon
fromWin32Icon Win32IconApplication = iDI_APPLICATION
fromWin32Icon Win32IconHand        = iDI_HAND
fromWin32Icon Win32IconQuestion    = iDI_QUESTION
fromWin32Icon Win32IconExclamation = iDI_EXCLAMATION
fromWin32Icon Win32IconAsterisk    = iDI_ASTERISK

data Win32Cursor = Win32CursorArrow
                 | Win32CursorIBeam
                 | Win32CursorWait
                 | Win32CursorCross
                 | Win32CursorUparrow
                 | Win32CursorSizeNWSE
                 | Win32CursorSizeNESW
                 | Win32CursorSizeWE
                 | Win32CursorSizeNS
                 deriving Eq

fromWin32Cursor :: Win32Cursor -> Cursor
fromWin32Cursor Win32CursorArrow    = iDC_ARROW
fromWin32Cursor Win32CursorIBeam    = iDC_IBEAM
fromWin32Cursor Win32CursorWait     = iDC_WAIT
fromWin32Cursor Win32CursorCross    = iDC_CROSS
fromWin32Cursor Win32CursorUparrow  = iDC_UPARROW
fromWin32Cursor Win32CursorSizeNWSE = iDC_SIZENWSE
fromWin32Cursor Win32CursorSizeNESW = iDC_SIZENESW
fromWin32Cursor Win32CursorSizeWE   = iDC_SIZEWE
fromWin32Cursor Win32CursorSizeNS   = iDC_SIZENS

class IsWin32GUIComponent a where
    render :: a -> Maybe HWND -> IO HWND

data Win32GUIComponent = forall a. IsWin32GUIComponent a => Win32GUIComponent a

instance IsWin32GUIComponent Win32GUIComponent where
    render (Win32GUIComponent a) = render a

data Win32Window = Win32Window Text [Win32WindowStyle] [Win32WindowProperty]

instance IsWin32GUIComponent Win32Window where
    render (Win32Window windowClassName windowStyle windowProperties) parentHWND = do
        mainInstance <- getModuleHandle Nothing

        let windowClass = mkClassName (Text.unpack windowClassName)

        _ <- registerClass
                ( cS_VREDRAW + cS_HREDRAW
                , mainInstance
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , windowClass
                )

        window <- createWindow
                    windowClass
                    ""
                    (foldl (.|.) 0 (map fromWin32WindowStyle windowStyle))
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    parentHWND
                    Nothing
                    mainInstance
                    typicalWindowProc

        mapM_ (flip applyProperty window) windowProperties

        _ <- showWindow window sW_SHOWNORMAL
        updateWindow window

        pure window

win32WindowTitle :: Text -> Writer [Win32WindowProperty] ()
win32WindowTitle = tell . pure . Win32WindowProperty . Win32WindowTitle

win32WindowIcon :: Win32Icon -> Writer [Win32WindowProperty] ()
win32WindowIcon = tell . pure . Win32WindowProperty . Win32WindowIcon

win32WindowCursor :: Win32Cursor -> Writer [Win32WindowProperty] ()
win32WindowCursor = tell . pure . Win32WindowProperty . Win32WindowCursor

win32WindowSize :: (Int, Int) -> Writer [Win32WindowProperty] ()
win32WindowSize = tell . pure . Win32WindowProperty . Win32WindowSize

win32WindowPosition :: (Int, Int) -> Writer [Win32WindowProperty] ()
win32WindowPosition = tell . pure . Win32WindowProperty . Win32WindowPosition

win32WindowBrush :: HBRUSH -> Writer [Win32WindowProperty] ()
win32WindowBrush = tell . pure . Win32WindowProperty . Win32WindowBrush

win32WindowChildren :: Writer [Win32GUIComponent] () -> Writer [Win32WindowProperty] ()
win32WindowChildren children =
    tell $ pure $ Win32WindowProperty $ Win32WindowChildren (snd $ runWriter children)

win32Window :: Text -> [Win32WindowStyle] -> Writer [Win32WindowProperty] () -> Writer [Win32GUIComponent] ()
win32Window windowClass windowStyle windowProperties =
    tell $ pure $ Win32GUIComponent $
        Win32Window windowClass windowStyle (snd $ runWriter windowProperties)

main :: IO ()
main = do
    callCommand "wpeinit"

    displayWidth  <- getSystemMetrics sM_CXSCREEN
    displayHeight <- getSystemMetrics sM_CYSCREEN
    brush         <- createSolidBrush (rgb 255 255 255)

    let testWindow =
            win32Window "HShell-Main" [Win32WindowStyleOverlapped] $ do
                win32WindowTitle "HShell"
                win32WindowIcon Win32IconQuestion
                win32WindowCursor Win32CursorIBeam
                win32WindowSize (displayWidth, displayHeight)
                win32WindowPosition (0, 0)
                win32WindowBrush brush
                win32WindowChildren $ do
                    win32Window "HShell-Sub" [Win32WindowStyleOverlapped, Win32WindowStyleChild] $ do
                        win32WindowTitle "HELLO"
                        win32WindowIcon Win32IconAsterisk
                        win32WindowCursor Win32CursorArrow
                        win32WindowSize (displayWidth `div` 2, displayHeight `div` 2)
                        win32WindowPosition (100, 100)
                        win32WindowBrush brush

    let a = head $ snd $ runWriter testWindow

    hwnd <- render a Nothing
    messagePump hwnd

typicalWindowProc :: LPPAINTSTRUCT -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
typicalWindowProc hwnd wmsg wParam lParam
    | wmsg == wM_DESTROY = sendMessage hwnd wM_QUIT 1 0 >> pure 0
    | otherwise          = defWindowProcSafe (Just hwnd) wmsg wParam lParam

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
