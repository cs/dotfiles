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
import           Decoration (Theme(..), decorateWindows)
import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Xlib
import           System.Exit
import           XMonad.Actions.CycleWS (nextScreen, shiftNextScreen)
import           XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Main (xmonad)
import           XMonad.ManageHook
import           XMonad.Operations
import qualified Data.Map as M
import qualified Polybar
import qualified Scratchpads
import qualified XMonad.StackSet as W

main :: IO ()
main = do
  let decoTheme = Theme { focusedColor     = "#f36b15"
                        , focusedTextColor = "#ffffff"
                        , normalColor      = "#000000"
                        , normalTextColor  = "#bbbbbb"
                        , fontName         = "xft:Noto Sans Mono:style=Medium,Regular:pixelsize=20"
                        , decoWidth        = 30000
                        , decoHeight       = 24 }

  xmonad $ ewmh $ XConfig
    { XMonad.borderWidth = 4
    , XMonad.clickJustFocuses = False
    , XMonad.clientMask = clientMask
    , XMonad.focusFollowsMouse = False
    , XMonad.focusedBorderColor = focusedColor decoTheme
    , XMonad.handleEventHook = mconcat [ (\_ -> return (All True))
                                       , docksEventHook
                                       , fullscreenEventHook ]
    , XMonad.handleExtraArgs = handleExtraArgs
    , XMonad.keys = keys
    , XMonad.layoutHook = decorateWindows decoTheme $ avoidStruts layout
    , XMonad.logHook = Polybar.logHook
    , XMonad.manageHook = mconcat [ manageDocks
                                  , Scratchpads.manageHook ]
    , XMonad.modMask = mod1Mask
    , XMonad.mouseBindings = mouseBindings
    , XMonad.normalBorderColor = normalColor decoTheme
    , XMonad.rootMask = rootMask
    , XMonad.startupHook = docksStartupHook <+> setWMName "LG3D"
    , XMonad.terminal = "alacritty"
    , XMonad.workspaces = fmap show [1..9] }

clientMask :: EventMask
clientMask = structureNotifyMask
         .|. enterWindowMask
         .|. propertyChangeMask

handleExtraArgs :: [String] -> XConfig Layout -> IO (XConfig Layout)
handleExtraArgs = \xs theConf -> case xs of
  [] -> return theConf
  _  -> fail ("unrecognized flags:" ++ show xs)

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Launch a new terminal:
  [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
  -- Launch a new web browser:
  , ((modMask .|. shiftMask, xK_Return), spawn "chromium")
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
  -- Swap the focused window and the master window:
  , ((modMask .|. shiftMask, xK_m), windows W.swapMaster)
  -- GMail Scratchpad:
  , ((modMask, xK_F1), Scratchpads.gmailAction)
  -- Twitter Scratchpad:
  , ((modMask, xK_F2), Scratchpads.twitterAction)
  -- Slack Scratchpad:
  , ((modMask, xK_F3), Scratchpads.slackAction)
  -- Telegram Scratchpad:
  , ((modMask, xK_F4), Scratchpads.telegramAction)
  -- Mute/Unmute audio output (Fn + F1):
  , ((noModMask, xF86XK_AudioMute),
     spawn "pamixer --toggle-mute")
  -- Decrease audio volume by 5% (Fn + F2):
  , ((noModMask, xF86XK_AudioLowerVolume),
     spawn "pamixer --decrease 5")
  -- Increase audio volume by 5% (Fn + F3):
  , ((noModMask, xF86XK_AudioRaiseVolume),
     spawn "pamixer --increase 5")
  -- Mute/Unmute audio input (Fn + F4):
  , ((noModMask, 0x1008FFB2), -- i.e. XF86AudioMicMute
     spawn "pamixer --default-source --toggle-mute")
  -- Decrease backlight by 10% (Fn + F5):
  , ((noModMask, xF86XK_MonBrightnessDown),
     spawn "xbacklight -dec 10%")
  -- Increase backlight by 10% (Fn + F6):
  , ((noModMask, xF86XK_MonBrightnessUp),
     spawn "xbacklight -inc 10%")
  -- ??? (Fn + F7):
  , ((noModMask, 0x1008FF59), -- i.e. XF86Display
     return ())
  -- ??? (Fn + F8):
  , ((noModMask, 0x1008FF95), -- i.e. XF86WLAN
     return ())
  -- ??? (Fn + F9):
  , ((noModMask, 0x1008FF81), -- i.e. XF86Tools
     return ())
  -- ??? (Fn + F10):
  , ((noModMask, 0x1008FF1B), -- i.e. XF86Search
     return ())
  -- ??? (Fn + F11):
  , ((noModMask, 0x1008FF4A), -- i.e. XF86LaunchA
     return ())
  -- ??? (Fn + F12):
  , ((noModMask, 0x1008FF5D), -- i.e. XF86Explorer
     return ())
  -- Quit xmonad:
  , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  -- Lock all screens:
  , ((modMask, xK_q), spawn "slock")
  -- Switch to next screen:
  , ((modMask, xK_w), nextScreen)
  -- Move window to next screen:
  , ((modMask .|. shiftMask, xK_w), shiftNextScreen >> nextScreen) ] ++
  -- Switch to workspace N:
  [ ((modMask, k), windows $ W.view i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ] ++
  -- Move window to workspace N:
  [ ((modMask .|. shiftMask, k), windows $ W.shift i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ]

layout = tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 16 $ ResizableTall nmaster delta ratio []
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
