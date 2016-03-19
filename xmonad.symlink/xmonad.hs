import XMonad

main :: IO ()
main = do
  xmonad defaultConfig
    { terminal = "urxvt"
    , borderWidth = 3
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#00ff00"
    , modMask = mod1Mask
    , workspaces = fmap show [1..9]
    }
