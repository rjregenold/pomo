module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Pomo.AppM (runAppM)
import Pomo.Component.Router as Router
import Pomo.Data.RunEnv as RunEnv
import Pomo.Env (Env)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
      runEnv = RunEnv.Dev
      env :: Env
      env = { runEnv }

      rootComponent = H.hoist (runAppM env) Router.component

  runUI rootComponent unit body
