import           XMonad.Core as XMonad hiding (
  borderWidth, clickJustFocuses, clientMask, focusFollowsMouse,
  focusedBorderColor, handleEventHook, handleExtraArgs, keys, layoutHook,
  logHook, manageHook, modMask, mouseBindings, normalBorderColor, rootMask,
  startupHook, terminal, workspaces )
import qualified XMonad.Core as XMonad (
  borderWidth, clickJustFocuses, clientMask, focusFollowsMouse,
  focusedBorderColor, handleEventHook, handleExtraArgs, keys, layoutHook,
  logHook, manageHook, modMask, mouseBindings, normalBorderColor, rootMask,
  startupHook, terminal, workspaces )

import           Data.Bits ((.|.))
import           Data.Monoid
import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Xlib
import           System.Exit
import           XMonad.Hooks.DynamicLog (xmobar)
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Main (xmonad)
import           XMonad.ManageHook
import           XMonad.Operations
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main :: IO ()
main = do
  xmonad =<< xmobar XConfig
    { XMonad.borderWidth = 3
    , XMonad.clickJustFocuses = True
    , XMonad.clientMask = clientMask
    , XMonad.focusFollowsMouse = True
    , XMonad.focusedBorderColor = "#00ff00"
    , XMonad.handleEventHook = (\_ -> return (All True)) <+> docksEventHook
    , XMonad.handleExtraArgs = handleExtraArgs
    , XMonad.keys = keys
    , XMonad.layoutHook = smartBorders $ avoidStruts layout
    , XMonad.logHook = return ()
    , XMonad.manageHook = manageDocks
    , XMonad.modMask = mod1Mask
    , XMonad.mouseBindings = mouseBindings
    , XMonad.normalBorderColor = "#333333"
    , XMonad.rootMask = rootMask
    , XMonad.startupHook = return ()
    , XMonad.terminal = "urxvt"
    , XMonad.workspaces = fmap show [1..9] }

clientMask :: EventMask
clientMask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask

handleExtraArgs = \ xs theConf -> case xs of
  [] -> return theConf
  _  -> fail ("unrecognized flags:" ++ show xs)

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Launch a new terminal:
  [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  -- Start program launcher:
  , ((modMask .|. shiftMask, xK_p), spawn "gmrun")
  -- Close the focused window:
  , ((modMask .|. shiftMask, xK_c), kill)
  -- Rotate through the available layout algorithms:
  , ((modMask, xK_space), sendMessage NextLayout)
  -- Reset the layouts on the current workspace to default:
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  -- Resize viewed windows to the correct size:
  , ((modMask, xK_n), refresh)
  -- Swap the focused window and the master window:
  , ((modMask, xK_Return), windows W.swapMaster)
  -- Swap the focused window with the next window:
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
  -- Swap the focused window with the previous window:
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
  -- Shrink the master area:
  , ((modMask, xK_h), sendMessage Shrink)
  -- Expand the master area:
  , ((modMask, xK_l), sendMessage Expand)
  -- Increment the number of windows in the master area:
  , ((modMask, xK_comma), sendMessage (IncMasterN 1))
  -- Deincrement the number of windows in the master area:
  , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
  -- Move focus to the next window:
  , ((modMask, xK_Tab), windows W.focusDown)
  -- Move focus to the previous window:
  , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp)
  -- Move focus to the next window:
  , ((modMask, xK_j), windows W.focusDown)
  -- Move focus to the previous window:
  , ((modMask, xK_k), windows W.focusUp)
  -- Move focus to the master window:
  , ((modMask, xK_m), windows W.focusMaster)
  -- Mute/Unmute audio output (Fn + F1):
  , ((noModMask, xF86XK_AudioMute),
     spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  -- Decrease audio volume by 10% (Fn + F2):
  , ((noModMask, xF86XK_AudioLowerVolume),
     spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  -- Increase audio volume by 10% (Fn + F3):
  , ((noModMask, xF86XK_AudioRaiseVolume),
     spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  -- Mute/Unmute audio input (Fn + F4):
  -- This doesn't work currently, but a fix is scheduled for linux 4.8.
  -- See https://sourceforge.net/p/ibm-acpi/mailman/message/34988427/
  , ((noModMask, xK_VoidSymbol), -- TODO: find correct keysym
     spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
  -- Decrease backlight by 10% (Fn + F5):
  , ((noModMask, xF86XK_MonBrightnessDown),
     spawn "xbacklight -dec 10%")
  -- Increase backlight by 10% (Fn + F6):
  , ((noModMask, xF86XK_MonBrightnessUp),
     spawn "xbacklight -inc 10%")
  -- ??? (Fn + F7):
  -- This doesn't work currently, but a fix is scheduled for linux 4.8.
  -- See https://sourceforge.net/p/ibm-acpi/mailman/message/34988427/
  , ((noModMask, xK_VoidSymbol), -- TODO: find correct keysym
     spawn "")
  -- ??? (Fn + F8):
  -- This doesn't work currently, but a fix is scheduled for linux 4.8.
  -- See https://sourceforge.net/p/ibm-acpi/mailman/message/34988427/
  , ((noModMask, xK_VoidSymbol), -- TODO: find correct keysym
     spawn "")
  -- ??? (Fn + F9):
  -- This doesn't work currently, but a fix is scheduled for linux 4.8.
  -- See https://sourceforge.net/p/ibm-acpi/mailman/message/34988427/
  , ((noModMask, xK_VoidSymbol), -- TODO: find correct keysym
     spawn "")
  -- ??? (Fn + F10):
  -- This doesn't work currently, but a fix is scheduled for linux 4.8.
  -- See https://sourceforge.net/p/ibm-acpi/mailman/message/34988427/
  , ((noModMask, xK_VoidSymbol), -- TODO: find correct keysym
     spawn "")
  -- ??? (Fn + F11):
  -- This doesn't work currently, but a fix is scheduled for linux 4.8.
  -- See https://sourceforge.net/p/ibm-acpi/mailman/message/34988427/
  , ((noModMask, xK_VoidSymbol), -- TODO: find correct keysym
     spawn "")
  -- Launch Nautilus (Fn + F12):
  -- This doesn't work currently, but a fix is scheduled for linux 4.8.
  -- See https://sourceforge.net/p/ibm-acpi/mailman/message/34988427/
  , ((noModMask, xK_VoidSymbol), -- TODO: find correct keysym
     spawn "nautilus")
  -- Quit xmonad:
  , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess)) ] ++
  -- Switch to workspace N:
  [ ((modMask, k), windows $ W.greedyView i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ] ++
  -- Move window to workspace N:
  [ ((modMask .|. shiftMask, k), windows $ W.shift i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ] ++
  -- Switch to physical/Xinerama screens 1, 2, or 3:
  [ ((modMask, k),
     screenWorkspace sc >>= flip whenJust (windows . W.view))
      | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..] ] ++
  -- Move window to screen 1, 2, or 3:
  [ ((modMask .|. shiftMask, k),
     screenWorkspace sc >>= flip whenJust (windows . W.shift))
      | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..] ]

layout = tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 5 $ ResizableTall nmaster delta ratio []
    -- Default number of windows in master pane
    nmaster = 1
    -- Percent of the screen to increment when resizing
    delta = 5/100
    -- Default proportion of the screen taken up by main pane
    ratio = toRational (2/(1 + sqrt 5 :: Double))

mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  -- Raise the window to the top of the stack:
  [ ((modMask, button2),
     windows . (W.shiftMaster .) . W.focusWindow) ]

rootMask :: EventMask
rootMask = substructureRedirectMask
        .|. substructureNotifyMask
        .|. enterWindowMask
        .|. leaveWindowMask
        .|. structureNotifyMask
        .|. buttonPressMask
