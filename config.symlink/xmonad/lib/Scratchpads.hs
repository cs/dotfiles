module Scratchpads
  ( manageHook
  , gmailAction
  , slackAction
  , trelloAction
  , twitterAction
  ) where

import           Prelude hiding (all)
import           XMonad hiding (manageHook)
import           XMonad.Util.ExclusiveScratchpads
import qualified XMonad.StackSet as W

manageHook :: ManageHook
manageHook = xScratchpadsManageHook all

gmailAction :: X ()
gmailAction = scratchpadAction all "gmail"

slackAction :: X ()
slackAction = scratchpadAction all "slack"

trelloAction :: X ()
trelloAction = scratchpadAction all "trello"

twitterAction :: X ()
twitterAction = scratchpadAction all "twitter"

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

all :: ExclusiveScratchpads
all = mkXScratchpads
        [ ("gmail", makeCommand tldGmail, makeQuery tldGmail)
        , ("slack", makeCommand tldSlack, makeQuery tldSlack)
        , ("trello", makeCommand tldTrello, makeQuery tldTrello)
        , ("twitter", makeCommand tldTwitter, makeQuery tldTwitter)
        ] $ customFloating $ W.RationalRect 0.05 0.05 0.90 0.90
  where tldGmail = "mail.google.com"
        tldSlack = "bugfactoryio.slack.com"
        tldTrello = "trello.com"
        tldTwitter = "twitter.com"
        makeQuery tld = appName =? tld
        makeCommand tld = "google-chrome-stable --app=https://" ++ tld
