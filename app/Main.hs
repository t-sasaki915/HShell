{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Lens               (makeLenses, over, (^.))
import           Data.Bits                  ((.|.))
import           Data.Text                  (append)
import qualified Data.Text                  as Text
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
    { _displayWidth  :: Int
    , _displayHeight :: Int
    , _clickedCount  :: Int
    } deriving Show

makeLenses ''Model

data Msg = ButtonClicked deriving Eq

instance IsModel Model
instance IsMsg Msg

init :: IO Model
init = do
    displayWidth'  <- getSystemMetrics sM_CXSCREEN
    displayHeight' <- getSystemMetrics sM_CYSCREEN

    pure $ Model
        { _displayWidth  = displayWidth'
        , _displayHeight = displayHeight'
        , _clickedCount  = 0
        }

update :: Msg -> Model -> IO Model
update ButtonClicked model =
    print model >>
        pure (over clickedCount (+1) model)

view :: Model -> GUIComponents
view model =
    window "HShell-Main" Normal $ do
        windowTitle "HShell"
        windowIcon (FromResource 101)
        windowCursor IBeam
        windowSize (model ^. displayWidth, model ^. displayHeight)
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
                windowSize (model ^. displayWidth `div` 2, model ^. displayHeight `div` 2)
                windowPosition (100, 100)
                windowBrush (SolidBrush 255 0 0)
                windowChildren $ do
                    button $ do
                        buttonLabel ("Clicked: " `append` Text.show (model ^. clickedCount))
                        buttonSize (100, 100)
                        buttonPosition (20, 50)

                    window "HShell-Sub-Sub" BorderlessChild $ do
                        windowTitle "GOOD MORNING"
                        windowIcon Application
                        windowCursor Wait
                        windowSize (50, 50)
                        windowPosition (0, 0)
                        windowBrush (SolidBrush 0 255 0)

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
main =
    wpeInit >>
        runTEA init update view
