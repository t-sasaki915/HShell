module Graphics.GUI.Component.Button.DSL
    ( buttonLabel
    , buttonSize
    , buttonPosition
    , button
    ) where
import           Control.Monad.Writer                   (MonadWriter (tell),
                                                         Writer, runWriter)
import           Data.Text                              (Text)
import           Graphics.GUI.Component                 (GUIComponent (..))
import           Graphics.GUI.Component.Button          (Button (Button))
import           Graphics.GUI.Component.Button.Property

buttonLabel :: Text -> Writer [ButtonProperty] ()
buttonLabel = tell . pure . ButtonProperty . ButtonLabel

buttonSize :: (Int, Int) -> Writer [ButtonProperty] ()
buttonSize = tell . pure . ButtonProperty . ButtonSize

buttonPosition :: (Int, Int) -> Writer [ButtonProperty] ()
buttonPosition = tell . pure . ButtonProperty . ButtonPosition

button :: Writer [ButtonProperty] () -> Writer [GUIComponent] ()
button = tell . pure . GUIComponent . Button . snd . runWriter
