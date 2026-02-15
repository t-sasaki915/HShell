{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Lens               (makeLenses, over, (^.))
import           Data.Text                  (append)
import qualified Data.Text                  as Text
import           Prelude                    hiding (init)
import           System.Exit                (exitFailure)
import           System.Process.Typed       (ExitCode (ExitSuccess), proc,
                                             runProcess)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)
import           TEAWin32.Application       (defaultSettings, runTEA)
import           TEAWin32.Effect            (showMessageBox)
import           TEAWin32.Effect.MessageBox
import           TEAWin32.GUI.DSL

data Model = Model
    { _displayWidth  :: Int
    , _displayHeight :: Int
    , _clickedCount  :: Int
    } deriving Show

makeLenses ''Model

data Msg = ButtonClicked deriving (Show, Eq)

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
    window_ "HShell-Main" "HShell-Main" Normal
        [ title_ "HShell"
        , icon_ (FromResource 101)
        , cursor_ IBeam
        , size_ (model ^. displayWidth, model ^. displayHeight)
        , position_ (0, 0)
        , backgroundColour_ (RGB 255 255 255)
        ] $ do
            button_ "TestButton" [title_ "TEST BUTTON", size_ (100, 50), position_ (0, 0), onClick_ ButtonClicked]

            window_ "HShell-Sub" "HShell-Sub" NormalChild
                [ title_ "HELLO"
                , icon_ Exclamation
                , cursor_ Arrow
                , size_ (model ^. displayWidth `div` 2, model ^. displayHeight `div` 2)
                , position_ (100, 100)
                , backgroundColour_ (RGB 255 0 0)
                ] $ do
                    button_ "TestButton2" [title_ ("Clicked: " `append` Text.show (model ^. clickedCount)), size_ (100, 100), position_ (20, 50)]

                    window_ "HShell-Sub-Sub" "HShell-Sub-Sub" BorderlessChild
                        [title_ "GOOD MORNING", icon_ Application, cursor_ Wait, size_ (50, 50), position_ (0, 0), backgroundColour_ (RGB 0 255 0)] noChildren

wpeInit :: IO ()
wpeInit = do
    try (runProcess (proc "X:\\Windows\\System32\\wpeinit.exe" [])) >>= \case
        Right ExitSuccess           -> pure ()
        Right x                     -> showMsgBox ("ExitCode: " <> Text.show x)
        Left (err :: SomeException) -> showMsgBox (Text.show err)

    where
        showMsgBox detail = do
            msgBoxResult <- showMessageBox defaultMessageBoxSettings
                    { messageBoxTitle   = "HShell"
                    , messageBoxContent = "Failed to initialise Windows PE. Continue?\n" <> detail
                    , messageBoxIcon    = MessageBoxIconError
                    , messageBoxButtons = MessageBoxButtonsYesNo
                    }

            case msgBoxResult of
                MessageBoxResultYes -> pure ()
                _                   -> exitFailure

main :: IO ()
main =
    wpeInit >>
        runTEA defaultSettings init update view
