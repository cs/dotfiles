module Scratchpads
  ( manageHook
  , gmailAction
  , slackAction
  , telegramAction
  , twitterAction
  ) where

import           Control.Monad (filterM)
import           Data.Maybe (listToMaybe)
import           XMonad hiding (manageHook)
import           XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import           XMonad.Hooks.ManageHelpers (doRectFloat)
import qualified XMonad.StackSet as W

hiddenWorkspaceTag :: String
hiddenWorkspaceTag = "NSP"

manageHook :: ManageHook
manageHook =
  composeAll $ fmap (\scrachpad -> query scrachpad --> hook) scratchpads
  where hook = doRectFloat $ W.RationalRect 0.05 0.05 0.90 0.90

gmailAction :: X ()
gmailAction = scratchpadAction scratchpads "gmail"

slackAction :: X ()
slackAction = scratchpadAction scratchpads "slack"

telegramAction :: X ()
telegramAction = scratchpadAction scratchpads "telegram"

twitterAction :: X ()
twitterAction = scratchpadAction scratchpads "twitter"

---------------------------------------------------------------------------------
---- Internal
---------------------------------------------------------------------------------

data Scratchpad = Scratchpad { name :: String, cmd :: String, query  :: Query Bool }

scratchpads :: [Scratchpad]
scratchpads = [ Scratchpad "gmail" (makeCommand tldGmail) (makeQuery tldGmail)
              , Scratchpad "slack" (makeCommand tldSlack) (makeQuery tldSlack)
              , Scratchpad "telegram" (makeCommand tldTelegram) (makeQuery tldTelegram)
              , Scratchpad "twitter" (makeCommand tldTwitter) (makeQuery tldTwitter)
              ]
  where tldGmail = "mail.google.com"
        tldSlack = "bugfactoryio.slack.com"
        tldTelegram = "web.telegram.org"
        tldTwitter = "twitter.com"
        makeQuery tld = appName =? tld
        makeCommand tld = "chromium --app=https://" ++ tld

scratchpadAction :: [Scratchpad] -> String -> X ()
scratchpadAction scratchpads n =
  case findByName scratchpads n of
    Just scratchpad -> withWindowSet $ \ws -> do
      filterCurrent <- filterM (runQuery (query scratchpad)) ((maybe [] W.integrate . W.stack . W.workspace . W.current) ws)
      filterAll <- filterM (runQuery (query scratchpad)) (W.allWindows ws)
      case filterCurrent of
        [] -> do
          case filterAll of
            [] -> spawn (cmd scratchpad)
            _  -> (windows . W.shiftWin (W.currentTag ws)) (head filterAll)
        _ -> do
          if null (filter ((== hiddenWorkspaceTag) . W.tag) (W.workspaces ws))
              then addHiddenWorkspace hiddenWorkspaceTag
              else return ()
          (windows . W.shiftWin hiddenWorkspaceTag) (head filterAll)
    Nothing         -> return ()
  where findByName :: [Scratchpad] -> String -> Maybe Scratchpad
        findByName scratchpads n = listToMaybe $ filter ((n==) . name) scratchpads
