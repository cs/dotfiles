module Decoration (Theme(..), decorateWindows) where

import           XMonad

data Theme = Theme { focusedColor     :: String
                   , focusedTextColor :: String
                   , normalColor      :: String
                   , normalTextColor  :: String
                   , fontName         :: String
                   , decoWidth        :: Dimension
                   , decoHeight       :: Dimension
                   } deriving (Show, Read)

decorateWindows :: Theme -> l a -> l a
decorateWindows theme layout = layout
