module Scratchpads (manageHook, trelloAction) where

import           Prelude hiding (all)
import           XMonad hiding (manageHook)
import           XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import        Debug.Trace

manageHook :: ManageHook
manageHook = namedScratchpadManageHook all

trelloAction :: X ()
trelloAction = namedScratchpadAction all (name trello)

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

all :: [NamedScratchpad]
all = [trello]

trello :: NamedScratchpad
trello = NS name cmd query hook
  where tld   = "trello.com"
        name  = tld
        cmd   = "google-chrome-stable --app=https://" ++ tld
        query = appName =? tld :: Query Bool
        hook  = customFloating $ W.RationalRect 0.05 0.05 0.90 0.90
