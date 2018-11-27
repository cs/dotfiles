module Polybar (logHook) where

import           Codec.Binary.UTF8.String (encodeString)
import           Data.List (isPrefixOf, stripPrefix)
import           Data.Maybe (fromMaybe)
import           XMonad hiding (logHook)
import           XMonad.Hooks.DynamicLog (ppCurrent, ppHidden, ppSort, ppUrgent, ppVisible, pprWindowSet)
import           XMonad.Hooks.UrgencyHook (readUrgents)
import           XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

logHook :: ScreenId -> X ()
logHook numScreens = do
  let screenIds = [0..numScreens-1]
  mapM_ logWorkspaces screenIds
  mapM_ logTitle screenIds

logWorkspaces :: ScreenId -> X ()
logWorkspaces screenId = do
  sort    <- filterAndSort
  urgents <- readUrgents
  winset  <- gets XMonad.windowset
  let pp = def { ppCurrent = (\w -> "[" ++ fromMaybe w (stripPrefix screenPrefix w) ++ "]")
               , ppVisible = (\w -> "[" ++ fromMaybe w (stripPrefix screenPrefix w) ++ "]")
               , ppHidden  = (\w -> fromMaybe w (stripPrefix screenPrefix w))
               , ppUrgent  = (\w -> fromMaybe w (stripPrefix screenPrefix w)) }

  let ws = pprWindowSet sort urgents pp winset
  io $ appendFile path (encodeString ws ++ "\n")
  where path = "/tmp/.xmonad-workspaces-" ++ show (toInteger screenId)
        screenPrefix = show (toInteger screenId) ++ "_"
        getWorkspaceId :: WindowSpace -> WorkspaceId
        getWorkspaceId (W.Workspace id _ _) = id
        filterAndSort :: X ([WindowSpace] -> [WindowSpace])
        filterAndSort = do
          let filterNSP = filter (\w -> getWorkspaceId w /= "NSP")
          let filterScreens = filter (\w -> screenPrefix `isPrefixOf` (getWorkspaceId w))
          ppSort def >>= (\sort -> return (sort . filterScreens . filterNSP))

logTitle :: ScreenId -> X ()
logTitle screenId = do
  winset <- gets XMonad.windowset
  wt     <- maybe (return "") (fmap show . getName) . W.peek $ winset
  io $ appendFile path (encodeString wt ++ "\n")
  where path = "/tmp/.xmonad-title-" ++ show (toInteger screenId)
