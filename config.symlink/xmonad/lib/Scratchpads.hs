module Scratchpads
  ( manageHook
  , linkedinAction
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

linkedinAction :: X ()
linkedinAction = namedScratchpadAction all (name linkedin)

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
all = [linkedin, slack, trello, twitter]

linkedin :: NamedScratchpad
linkedin = NS name cmd query hook
  where tld   = "linkedin.com"
        name  = tld
        cmd   = "google-chrome-stable --app=https://" ++ tld
        query = appName =? tld :: Query Bool
        hook  = customFloating $ W.RationalRect 0.05 0.05 0.90 0.90

slack :: NamedScratchpad
slack = NS name cmd query hook
  where tld   = "bugfactoryio.slack.com"
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
