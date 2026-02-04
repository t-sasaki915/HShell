{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component
    ( GUIComponents
    , IsGUIComponent (..)
    , GUIComponent (..)
    ) where

import           Control.Monad.Writer (Writer)
import           Data.Data            (Typeable, cast)
import qualified Graphics.Win32       as Win32

type GUIComponents = Writer [GUIComponent] ()

class Eq a => IsGUIComponent a where
    render :: a -> Maybe Win32.HWND -> IO Win32.HWND

data GUIComponent = forall a. (Typeable a, Eq a, IsGUIComponent a) => GUIComponent a

instance Eq GUIComponent where
    (GUIComponent a) == (GUIComponent b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance IsGUIComponent GUIComponent where
    render (GUIComponent a) = render a
