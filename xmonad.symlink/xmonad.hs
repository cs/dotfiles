import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

main :: IO ()
main = do
  xmonad =<< xmobar defaultConfig
    { terminal = "urxvt"
    , borderWidth = 3
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#00ff00"
    , modMask = mod1Mask
    , keys = awesomeKeys
    , workspaces = fmap show [1..9]
    , layoutHook = smartBorders $ avoidStruts awesomeLayout
    , manageHook = manageHook defaultConfig <+> manageDocks
    , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
    }

awesomeLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 5 $ ResizableTall nmaster delta ratio []
    -- Default number of windows in master pane
    nmaster = 1
    -- Percent of the screen to increment when resizing
    delta = 5/100
    -- Default proportion of the screen taken up by main pane
    ratio = toRational (2/(1 + sqrt 5 :: Double))

awesomeKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
awesomeKeys conf = M.unions [ defaultKeys conf
                            , layoutKeys conf
                            , focusKeys conf
                            , workspaceKeys conf ]

defaultKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
defaultKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]

layoutKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
layoutKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Rotate through the available layout algorithms:
  [ ((modMask              , xK_space ), sendMessage NextLayout)
  -- Reset the layouts on the current workspace to default:
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
  -- Resize viewed windows to the correct size:
  , ((modMask              , xK_n     ), refresh)
  -- Swap the focused window and the master window:
  , ((modMask              , xK_Return), windows W.swapMaster)
  -- Swap the focused window with the next window:
  , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)
  -- Swap the focused window with the previous window:
  , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)
  -- Shrink the master area:
  , ((modMask              , xK_h     ), sendMessage Shrink)
  -- Expand the master area:
  , ((modMask              , xK_l     ), sendMessage Expand)
  -- Increment the number of windows in the master area:
  , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
  -- Deincrement the number of windows in the master area:
  , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) ]

focusKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
focusKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Move focus to the next window:
  [ ((modMask              , xK_Tab   ), windows W.focusDown)
  -- Move focus to the previous window:
  , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
  -- Move focus to the next window:
  , ((modMask              , xK_j     ), windows W.focusDown)
  -- Move focus to the previous window:
  , ((modMask              , xK_k     ), windows W.focusUp)
  -- Move focus to the master window:
  , ((modMask              , xK_m     ), windows W.focusMaster) ]

workspaceKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
workspaceKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Switch to workspace N:
  [ ((modMask              , k        ), windows $ W.greedyView i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ] ++
  -- Move window to workspace N:
  [ ((modMask .|. shiftMask, k        ), windows $ W.shift i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ] ++
  -- Switch to physical/Xinerama screens 1, 2, or 3:
  [ ((modMask              , k        ),
     screenWorkspace sc >>= flip whenJust (windows . W.view))
      | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..] ] ++
  -- Move window to screen 1, 2, or 3:
  [ ((modMask .|. shiftMask, k        ),
     screenWorkspace sc >>= flip whenJust (windows . W.shift))
      | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..] ]
