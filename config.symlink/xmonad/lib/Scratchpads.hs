module Scratchpads
  ( manageHook
  , slackAction
  , trelloAction
  , twitterAction
  ) where

import           Prelude hiding (all)
import           XMonad hiding (manageHook)
import           XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

manageHook :: ManageHook
manageHook = namedScratchpadManageHook all

slackAction :: X ()
slackAction = namedScratchpadAction all (name slack)

trelloAction :: X ()
trelloAction = namedScratchpadAction all (name trello)

twitterAction :: X ()
twitterAction = namedScratchpadAction all (name twitter)

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

all :: [NamedScratchpad]
all = [slack, trello, twitter]

slack :: NamedScratchpad
slack = NS name cmd query hook
  where tld   = "slack.com"
        name  = tld
        cmd   = "google-chrome-stable --app=https://" ++ tld ++ "/signin"
        query = appName =? (tld ++ "__signin") :: Query Bool
        hook  = customFloating $ W.RationalRect 0.05 0.05 0.90 0.90

trello :: NamedScratchpad
trello = NS name cmd query hook
  where tld   = "trello.com"
        name  = tld
        cmd   = "google-chrome-stable --app=https://" ++ tld
        query = appName =? tld :: Query Bool
        hook  = customFloating $ W.RationalRect 0.05 0.05 0.90 0.90

twitter :: NamedScratchpad
twitter = NS name cmd query hook
  where tld   = "twitter.com"
        name  = tld
        cmd   = "google-chrome-stable --app=https://" ++ tld
        query = appName =? tld :: Query Bool
        hook  = customFloating $ W.RationalRect 0.05 0.05 0.90 0.90
