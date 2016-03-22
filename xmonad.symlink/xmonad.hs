import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

main :: IO ()
main = do
  xmonad =<< xmobar defaultConfig
    { terminal = "urxvt"
    , borderWidth = 3
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#00ff00"
    , modMask = mod1Mask
    , workspaces = fmap show [1..9]
    , layoutHook = smartBorders $ avoidStruts awesomeLayout
    , manageHook = manageHook defaultConfig <+> manageDocks
    , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
    }

awesomeLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 10 $ ResizableTall nmaster delta ratio []
    -- Default number of windows in master pane
    nmaster = 1
    -- Percent of the screen to increment when resizing
    delta = 5/100
    -- Default proportion of the screen taken up by main pane
    ratio = toRational (2/(1 + sqrt 5 :: Double))
