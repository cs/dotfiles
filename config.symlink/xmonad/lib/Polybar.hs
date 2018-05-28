module Polybar (logHook) where

import           Codec.Binary.UTF8.String (encodeString)
import           XMonad hiding (logHook)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.UrgencyHook (readUrgents)
import           XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

logHook :: X ()
logHook = logWorkspaces def >> logTitle

logWorkspaces :: PP -> X ()
logWorkspaces pp = do
  winset <- gets XMonad.windowset
  urgents <- readUrgents
  sort' <- ppSort pp
  let ws = pprWindowSet sort' urgents pp winset
  io $ appendFile "/tmp/.xmonad-workspace-log" (encodeString ws ++ "\n")

logTitle :: X ()
logTitle = do
  winset <- gets XMonad.windowset
  wt <- maybe (return "") (fmap show . getName) . W.peek $ winset
  io $ appendFile "/tmp/.xmonad-title-log" (encodeString wt ++ "\n")
