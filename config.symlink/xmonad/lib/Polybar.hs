module Polybar (logHook) where

import           Control.Monad (join)
import           Data.Function (on)
import           Data.List (sortBy)
import           XMonad hiding (logHook)
import           XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

logHook :: X ()
logHook = do
  winset <- gets XMonad.windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = fmap W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))
