{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (void, when)
import           Control.Monad.Writer.Lazy  (runWriter)
import           Data.Bits                  ((.|.))
import           Graphics.GUI.Component     (IsGUIComponent (render))
import           Graphics.GUI.DSL
import           Graphics.Win32             (allocaMessage, dispatchMessage,
                                             getMessage, mB_ICONSTOP, mB_YESNO,
                                             messageBox, translateMessage)
import           System.Exit                (exitFailure)
import           System.Process.Typed       (ExitCode (ExitSuccess), proc,
                                             runProcess)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)

wpeInit :: IO ()
wpeInit = do
    try (runProcess (proc "X:\\Windows\\System32\\wpeinit.exe" [])) >>= \case
        Right ExitSuccess           -> pure ()
        Right x                     -> showMessageBox ("ExitCode: " ++ show x)
        Left (err :: SomeException) -> showMessageBox (show err)

    where
        showMessageBox detail =
            messageBox Nothing ("Failed to initialise Windows PE. Continue?\n" ++ detail) "HShell" (mB_ICONSTOP .|. mB_YESNO) >>= \case
                6 -> pure ()
                _ -> exitFailure

main :: IO ()
main = do
    wpeInit

    displayWidth  <- getSystemMetrics sM_CXSCREEN
    displayHeight <- getSystemMetrics sM_CYSCREEN

    let mainWindow =
            window "HShell-Main" [Popup] $ do
                windowTitle "HShell"
                windowIcon Question
                windowCursor IBeam
                windowSize (displayWidth, displayHeight)
                windowPosition (0, 0)
                windowBrush (SolidBrush 255 255 255)
                windowChildren $ do
                    button $ do
                        buttonLabel "TEST BUTTON"
                        buttonSize (100, 50)
                        buttonPosition (0, 0)

                    window "HShell-Sub" [Overlapped, Child] $ do
                        windowTitle "HELLO"
                        windowIcon (FromResource 102)
                        windowCursor Arrow
                        windowSize (displayWidth `div` 2, displayHeight `div` 2)
                        windowPosition (100, 100)
                        windowBrush (SolidBrush 255 0 0)
                        windowChildren $ do
                            button $ do
                                buttonLabel "TEST BUTTON 2"
                                buttonSize (100, 100)
                                buttonPosition (20, 50)

                            window "HShell-Sub-Sub" [Overlapped, Child] $ do
                                windowTitle "GOOD MORNING"
                                windowIcon Application
                                windowCursor Wait
                                windowSize (50, 0)
                                windowPosition (0, 0)
                                windowBrush (SolidBrush 0 255 0)

    let a = head $ snd $ runWriter mainWindow

    _ <- render a Nothing

    messagePump

messagePump :: IO ()
messagePump =
    allocaMessage $ \msg ->
        let pump =
                (try $ getMessage msg Nothing :: IO (Either SomeException Bool)) >>= \r ->
                    when (or r) $
                        void $ translateMessage msg >>
                            dispatchMessage msg >>
                                pump
        in pump
