module Graphics.GUI.Component.Window.DSL
    ( windowTitle
    , windowIcon
    , windowCursor
    , windowSize
    , windowPosition
    , windowBrush
    , windowChildren
    , window
    ) where

import           Control.Monad.Writer                   (MonadWriter (tell),
                                                         Writer, runWriter)
import           Data.Text                              (Text)
import           Graphics.GUI                           (Brush, Cursor, Icon,
                                                         WindowStyle)
import           Graphics.GUI.Component                 (GUIComponent (GUIComponent),
                                                         GUIComponents)
import           Graphics.GUI.Component.Window          (Window (Window))
import           Graphics.GUI.Component.Window.Property

windowTitle :: Text -> Writer [WindowProperty] ()
windowTitle = tell . pure . WindowProperty . WindowTitle

windowIcon :: Icon -> Writer [WindowProperty] ()
windowIcon = tell . pure . WindowProperty . WindowIcon

windowCursor :: Cursor -> Writer [WindowProperty] ()
windowCursor = tell . pure . WindowProperty . WindowCursor

windowSize :: (Int, Int) -> Writer [WindowProperty] ()
windowSize = tell . pure . WindowProperty . WindowSize

windowPosition :: (Int, Int) -> Writer [WindowProperty] ()
windowPosition = tell . pure . WindowProperty . WindowPosition

windowBrush :: Brush -> Writer [WindowProperty] ()
windowBrush = tell . pure . WindowProperty . WindowBrush

windowChildren :: Writer [GUIComponent] () -> Writer [WindowProperty] ()
windowChildren children =
    tell $ pure $ WindowProperty $ WindowChildren (snd $ runWriter children)

window :: Text -> WindowStyle -> Writer [WindowProperty] () -> GUIComponents
window windowClass windowStyle windowProperties =
    tell $ pure $ GUIComponent $
        Window windowClass windowStyle (snd $ runWriter windowProperties)
