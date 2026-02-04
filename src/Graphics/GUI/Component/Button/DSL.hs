module Graphics.GUI.Component.Button.DSL
    ( buttonLabel
    , buttonSize
    , buttonPosition
    , buttonClicked
    , button
    ) where
import           Control.Monad.Writer                   (MonadWriter (tell),
                                                         Writer, runWriter)
import           Data.Data                              (Typeable)
import           Data.Text                              (Text)
import           Framework.TEA                          (IsMsg)
import           Graphics.GUI.Component                 (GUIComponent (..),
                                                         GUIComponents)
import           Graphics.GUI.Component.Button          (Button (Button))
import           Graphics.GUI.Component.Button.Property

buttonLabel :: Text -> Writer [ButtonProperty] ()
buttonLabel = tell . pure . ButtonProperty . ButtonLabel

buttonSize :: (Int, Int) -> Writer [ButtonProperty] ()
buttonSize = tell . pure . ButtonProperty . ButtonSize

buttonPosition :: (Int, Int) -> Writer [ButtonProperty] ()
buttonPosition = tell . pure . ButtonProperty . ButtonPosition

buttonClicked :: (Typeable a, IsMsg a) => a -> Writer [ButtonProperty] ()
buttonClicked = tell . pure . ButtonProperty . ButtonClicked

button :: Writer [ButtonProperty] () -> GUIComponents
button = tell . pure . GUIComponent . Button . snd . runWriter
