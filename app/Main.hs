{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (when)
import           Control.Monad.Reader       (Reader)
import           Control.Monad.Writer.Lazy  (MonadWriter (tell), Writer,
                                             WriterT (runWriterT), runWriter,
                                             runWriterT)
import           Data.Functor               ((<&>))
import           Data.List.Extra            (firstJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Graphics.Win32             hiding (try)
import           System.Process             (callCommand)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.DLL           (getModuleHandle)
import           System.Win32.Info.Computer (getSystemMetrics)

type Win32GUIComponentDSL = WriterT [Win32GUIComponent] (Reader Win32GUIComponent)

data Win32GUIComponentProperty = forall a. IsWin32GUIComponentProperty a => Win32GUIComponentProperty a

data Win32WindowProperty = forall a. IsWin32WindowProperty a => Win32WindowProperty a

class IsWin32GUIComponentProperty a

class IsWin32WindowProperty a

data Win32WindowIcon            = Win32WindowIcon Icon
data Win32WindowCursor          = Win32WindowCursor Cursor
data Win32WindowSize            = Win32WindowSize (Int, Int)
data Win32WindowInitialPosition = Win32WindowInitialPosition (Int, Int)
data Win32WindowBrush           = Win32WindowBrush HBRUSH
data Win32WindowChildren        = Win32WindowChildren (Reader (Maybe HWND) [Win32GUIComponent])

instance IsWin32GUIComponentProperty Win32WindowIcon
instance IsWin32GUIComponentProperty Win32WindowCursor
instance IsWin32GUIComponentProperty Win32WindowSize
instance IsWin32GUIComponentProperty Win32WindowInitialPosition
instance IsWin32GUIComponentProperty Win32WindowBrush
instance IsWin32GUIComponentProperty Win32WindowChildren

instance IsWin32WindowProperty Win32WindowIcon
instance IsWin32WindowProperty Win32WindowCursor
instance IsWin32WindowProperty Win32WindowSize
instance IsWin32WindowProperty Win32WindowInitialPosition
instance IsWin32WindowProperty Win32WindowBrush
instance IsWin32WindowProperty Win32WindowChildren

data Win32WindowStyle = Win32WindowStylePopup

fromWin32WindowStyle :: Win32WindowStyle -> WindowStyle
fromWin32WindowStyle Win32WindowStylePopup = wS_POPUP

class IsWin32GUIComponent a where
    render :: a -> IO HWND

data Win32GUIComponent = forall a. IsWin32GUIComponent a => Win32GUIComponent a

data Win32Window = Win32Window Text Text Win32WindowStyle [Win32WindowProperty]

instance IsWin32GUIComponent Win32Window where
    render (Win32Window windowClassName windowTitle windowStyle windowProperties) = do
        {-mainInstance <- getModuleHandle Nothing

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
        -}
        pure ()

win32WindowIcon :: Icon -> Writer [Win32WindowProperty] ()
win32WindowIcon = tell . pure . Win32WindowProperty . Win32WindowIcon

win32WindowCursor :: Cursor -> Writer [Win32WindowProperty] ()
win32WindowCursor = tell . pure . Win32WindowProperty . Win32WindowCursor

win32WindowSize :: (Int, Int) -> Writer [Win32WindowProperty] ()
win32WindowSize = tell . pure . Win32WindowProperty . Win32WindowSize

win32WindowInitialPosition :: (Int, Int) -> Writer [Win32WindowProperty] ()
win32WindowInitialPosition = tell . pure . Win32WindowProperty . Win32WindowInitialPosition

win32WindowBrush :: HBRUSH -> Writer [Win32WindowProperty] ()
win32WindowBrush = tell . pure . Win32WindowProperty . Win32WindowBrush

win32WindowChildren :: WriterT [Win32GUIComponent] (Reader (Maybe HWND)) () -> Writer [Win32WindowProperty] ()
win32WindowChildren children =
    tell $ pure $ Win32WindowProperty $ Win32WindowChildren (snd <$> runWriterT children)

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

    {-let testWindow =
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
                []-}

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
