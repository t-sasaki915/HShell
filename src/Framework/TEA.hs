module Framework.TEA
    ( GUIComponents
    , IsModel
    , IsMsg
    , runTEA
    ) where

import           Control.Exception      (SomeException, try)
import           Control.Monad          (forM_, void, when)
import           Control.Monad.Writer   (runWriter)
import           Data.IORef             (atomicModifyIORef')
import           Framework.TEA.Internal
import           Graphics.GUI.Component (GUIComponents, IsGUIComponent (render))
import qualified Graphics.Win32         as Win32
import           Prelude                hiding (init)

runTEA :: (IsModel model, IsMsg msg) => IO model -> (msg -> model -> IO model) -> (model -> GUIComponents) -> IO ()
runTEA init _ view = do
    initModel <- init

    _ <- atomicModifyIORef' modelRef (const (Model initModel, Model initModel))

    -- TODO

    let initGUIComponents = fmap snd runWriter (view initModel)

    forM_ initGUIComponents $ \guiComponent ->
        render guiComponent Nothing

    messagePump

messagePump :: IO ()
messagePump =
    Win32.allocaMessage $ \msg ->
        let pump =
                (try $ Win32.getMessage msg Nothing :: IO (Either SomeException Bool)) >>= \r ->
                    when (or r) $
                        void $ Win32.translateMessage msg >>
                            Win32.dispatchMessage msg >>
                                pump
        in pump
