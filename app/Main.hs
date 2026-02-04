{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Data.Bits                  ((.|.))
import           Framework.TEA              (GUIComponents, IsModel, IsMsg,
                                             runTEA)
import           Graphics.GUI.DSL
import           Graphics.Win32             (mB_ICONSTOP, mB_YESNO, messageBox)
import           Prelude                    hiding (init)
import           System.Exit                (exitFailure)
import           System.Process.Typed       (ExitCode (ExitSuccess), proc,
                                             runProcess)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)

data Model = Model
    { displayWidth  :: Int
    , displayHeight :: Int
    }

instance IsModel Model

data Msg = ButtonClicked deriving (Eq, Show)

instance IsMsg Msg

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

init :: IO Model
init = do
    displayWidth'  <- getSystemMetrics sM_CXSCREEN
    displayHeight' <- getSystemMetrics sM_CYSCREEN

    pure $ Model
        { displayWidth  = displayWidth'
        , displayHeight = displayHeight'
        }

update :: Msg -> Model -> IO Model
update ButtonClicked model = putStrLn "CLICKED!!!!!!!!!!!!!" >> pure model

view :: Model -> GUIComponents
view model =
    window "HShell-Main" Normal $ do
        windowTitle "HShell"
        windowIcon (FromResource 101)
        windowCursor IBeam
        windowSize (displayWidth model, displayHeight model)
        windowPosition (0, 0)
        windowBrush (SolidBrush 255 255 255)
        windowChildren $ do
            button $ do
                buttonLabel "TEST BUTTON"
                buttonSize (100, 50)
                buttonPosition (0, 0)
                buttonClicked ButtonClicked

            window "HShell-Sub" NormalChild $ do
                windowTitle "HELLO"
                windowIcon Exclamation
                windowCursor Arrow
                windowSize (displayWidth model `div` 2, displayHeight model `div` 2)
                windowPosition (100, 100)
                windowBrush (SolidBrush 255 0 0)
                windowChildren $ do
                    button $ do
                        buttonLabel "TEST BUTTON 2"
                        buttonSize (100, 100)
                        buttonPosition (20, 50)

                    window "HShell-Sub-Sub" BorderlessChild $ do
                        windowTitle "GOOD MORNING"
                        windowIcon Application
                        windowCursor Wait
                        windowSize (50, 50)
                        windowPosition (0, 0)
                        windowBrush (SolidBrush 0 255 0)

main :: IO ()
main =
    wpeInit >>
        runTEA init update view
