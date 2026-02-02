{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -Wno-unsupported-calling-conventions #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (forM_, void, when)
import           Control.Monad.Reader       (MonadIO (liftIO), Reader,
                                             ReaderT (runReaderT), runReader)
import           Control.Monad.Writer.Lazy  (MonadWriter (tell), Writer,
                                             WriterT, runWriter, runWriterT)
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

newtype Win32WindowIcon     = Win32WindowIcon Icon
newtype Win32WindowCursor   = Win32WindowCursor Cursor
newtype Win32WindowSize     = Win32WindowSize (Int, Int)
newtype Win32WindowPosition = Win32WindowPosition (Int, Int)
newtype Win32WindowBrush    = Win32WindowBrush HBRUSH
newtype Win32WindowChildren = Win32WindowChildren [Win32GUIComponent]

instance IsWin32GUIComponentProperty Win32WindowIcon
instance IsWin32GUIComponentProperty Win32WindowCursor
instance IsWin32GUIComponentProperty Win32WindowSize
instance IsWin32GUIComponentProperty Win32WindowPosition
instance IsWin32GUIComponentProperty Win32WindowBrush
instance IsWin32GUIComponentProperty Win32WindowChildren

instance IsWin32WindowProperty Win32WindowIcon where
    applyProperty (Win32WindowIcon icon) windowHWND =
        loadIcon Nothing icon >>= \icon' ->
            void $ c_SetClassLongPtr windowHWND (-14) icon'

instance IsWin32WindowProperty Win32WindowCursor where
    applyProperty (Win32WindowCursor cursor) windowHWND =
        loadCursor Nothing cursor >>= \cursor' ->
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
    applyProperty (Win32WindowChildren children) windowHWND =
        forM_ children $ \(Win32GUIComponent child) ->
            runReaderT (render child) (Just windowHWND)

data Win32WindowStyle = Win32WindowStylePopup
                      | Win32WindowStyleOverlapped

fromWin32WindowStyle :: Win32WindowStyle -> WindowStyle
fromWin32WindowStyle Win32WindowStylePopup      = wS_POPUP
fromWin32WindowStyle Win32WindowStyleOverlapped = wS_OVERLAPPEDWINDOW

class IsWin32GUIComponent a where
    render :: a -> ReaderT (Maybe HWND) IO HWND

data Win32GUIComponent = forall a. IsWin32GUIComponent a => Win32GUIComponent a

instance IsWin32GUIComponent Win32GUIComponent where
    render (Win32GUIComponent a) = render a

data Win32Window = Win32Window Text Text Win32WindowStyle [Win32WindowProperty]

instance IsWin32GUIComponent Win32Window where
    render (Win32Window windowClassName windowTitle windowStyle windowProperties) = do
        mainInstance <- liftIO $ getModuleHandle Nothing

        let windowClass = mkClassName (Text.unpack windowClassName)

        _ <- liftIO $ registerClass
                ( cS_VREDRAW + cS_HREDRAW
                , mainInstance
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , windowClass
                )

        window <- liftIO $ createWindow
                    windowClass
                    (Text.unpack windowTitle)
                    (fromWin32WindowStyle windowStyle)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    mainInstance
                    typicalWindowProc

        liftIO $ mapM_ (flip applyProperty window) windowProperties

        _ <- liftIO $ showWindow window sW_SHOWNORMAL
        liftIO $ updateWindow window

        pure window

win32WindowIcon :: Icon -> Writer [Win32WindowProperty] ()
win32WindowIcon = tell . pure . Win32WindowProperty . Win32WindowIcon

win32WindowCursor :: Cursor -> Writer [Win32WindowProperty] ()
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

win32Window :: Text -> Text -> Win32WindowStyle -> Writer [Win32WindowProperty] () -> WriterT [Win32GUIComponent] (Reader (Maybe HWND)) ()
win32Window windowClass windowTitle windowStyle windowProperties =
    tell $ pure $ Win32GUIComponent $
        Win32Window windowClass windowTitle windowStyle (snd $ runWriter windowProperties)

main :: IO ()
main = do
    callCommand "wpeinit"

    displayWidth  <- getSystemMetrics sM_CXSCREEN
    displayHeight <- getSystemMetrics sM_CYSCREEN
    brush         <- createSolidBrush (rgb 255 255 255)

    let testWindow =
            win32Window "HShell-Main" "HShell" Win32WindowStylePopup $ do
                win32WindowIcon iDI_QUESTION
                win32WindowCursor iDC_IBEAM
                win32WindowSize (displayWidth, displayHeight)
                win32WindowPosition (0, 0)
                win32WindowBrush brush

    let a = head $ snd $ runReader (runWriterT testWindow) Nothing

    hwnd <- runReaderT (render a) Nothing
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
