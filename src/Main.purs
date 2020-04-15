module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Pomo.AppM (runAppM)
import Pomo.Component.Router as Router
import Pomo.Data.RunEnv as RunEnv
import Pomo.Data.TimerSettings (defaultTimerSettings)
import Pomo.Data.AssetUrls (AssetUrls)

main :: AssetUrls -> Effect Unit
main assetUrls = HA.runHalogenAff do
  body <- HA.awaitBody
  timerSettings <- maybe (throwError (error "invalid timer settings")) pure defaultTimerSettings
  let env = { runEnv: RunEnv.Dev, assetUrls,  timerSettings }
      rootComponent = H.hoist (runAppM env) Router.component
  runUI rootComponent {} body
