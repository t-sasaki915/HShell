{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (when)
import           Control.Monad.Writer.Lazy  (runWriter)
import           Graphics.GUI.Component     (IsGUIComponent (render))
import           Graphics.GUI.DSL
import           Graphics.Win32             (HWND, allocaMessage,
                                             dispatchMessage, getMessage,
                                             translateMessage)
import           System.Process             (callCommand)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)

main :: IO ()
main = do
    callCommand "wpeinit"

    displayWidth  <- getSystemMetrics sM_CXSCREEN
    displayHeight <- getSystemMetrics sM_CYSCREEN

    let mainWindow =
            window "HShell-Main" [Overlapped] $ do
                windowTitle "HShell"
                windowIcon Question
                windowCursor IBeam
                windowSize (displayWidth, displayHeight)
                windowPosition (0, 0)
                windowBrush (SolidBrush 255 255 255)
                windowChildren $ do
                    window "HShell-Sub" [Overlapped, Child] $ do
                        windowTitle "HELLO"
                        windowIcon Asterisk
                        windowCursor Arrow
                        windowSize (displayWidth `div` 2, displayHeight `div` 2)
                        windowPosition (100, 100)
                        windowBrush (SolidBrush 255 0 0)
                        windowChildren $ do
                            window "HShell-Sub-Sub" [Overlapped, Child] $ do
                                windowTitle "GOOD MORNING"
                                windowIcon Application
                                windowCursor Wait
                                windowSize (50, 0)
                                windowPosition (0, 0)
                                windowBrush (SolidBrush 0 255 0)

    let a = head $ snd $ runWriter mainWindow

    hwnd <- render a Nothing
    messagePump hwnd

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
