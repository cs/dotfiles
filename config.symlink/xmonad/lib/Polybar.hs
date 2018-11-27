module Polybar (logHook) where

import           Codec.Binary.UTF8.String (encodeString)
import           System.Directory (doesFileExist)
import           XMonad hiding (logHook)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.UrgencyHook (readUrgents)
import           XMonad.Layout.IndependentScreens (countScreens)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Run (safeSpawn)
import qualified XMonad.StackSet as W

logHook :: ScreenId -> X ()
logHook numScreens = do
  let screenIds = [0..numScreens-1]
  mapM_ logWorkspaces screenIds
  mapM_ logTitle screenIds

logWorkspaces :: ScreenId -> X ()
logWorkspaces screenId = do
  winset  <- gets XMonad.windowset
  urgents <- readUrgents
  sort'   <- ppSort (namedScratchpadFilterOutWorkspacePP def)
  let ws = pprWindowSet sort' urgents def winset
  appendFifo fifoPath (encodeString ws)
  where fifoPath = "/tmp/.xmonad-workspaces-" ++ show (toInteger screenId)

logTitle :: ScreenId -> X ()
logTitle screenId = do
  winset <- gets XMonad.windowset
  wt     <- maybe (return "") (fmap show . getName) . W.peek $ winset
  appendFifo fifoPath (encodeString wt)
  where fifoPath = "/tmp/.xmonad-title-" ++ show (toInteger screenId)

appendFifo :: FilePath -> String -> X ()
appendFifo path payload = do
  exist <- io $ doesFileExist path
  if exist then return () else safeSpawn "mkfifo" [path]
  io $ appendFile path (payload ++ "\n")
