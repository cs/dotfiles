module Polybar (logHook) where

import           Codec.Binary.UTF8.String (encodeString)
import           XMonad hiding (logHook)
import           XMonad.Hooks.DynamicLog (ppSort, pprWindowSet)
import           XMonad.Hooks.UrgencyHook (readUrgents)
import           XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

logHook :: X ()
logHook = logWorkspaces >> logTitle

logWorkspaces :: X ()
logWorkspaces = do
  sort    <- filterAndSort
  urgents <- readUrgents
  winset  <- gets XMonad.windowset

  let ws = pprWindowSet sort urgents def winset
  io $ appendFile path (encodeString ws ++ "\n")
  where path = "/tmp/.xmonad-workspaces-log"
        getWorkspaceId :: WindowSpace -> WorkspaceId
        getWorkspaceId (W.Workspace id _ _) = id
        filterAndSort :: X ([WindowSpace] -> [WindowSpace])
        filterAndSort = do
          let removeNSP = filter (\w -> getWorkspaceId w /= "NSP")
          ppSort def >>= (\sort -> return (sort . removeNSP))

logTitle :: X ()
logTitle = do
  winset <- gets XMonad.windowset
  wt     <- maybe (return "") (fmap show . getName) . W.peek $ winset
  io $ appendFile path (encodeString wt ++ "\n")
  where path = "/tmp/.xmonad-title-log"
