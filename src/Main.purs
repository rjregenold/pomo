module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Pomo.AppM (runAppM)
import Pomo.Component.Router as Router
import Pomo.Data.Route (routeCodec)
import Pomo.Data.RunEnv as RunEnv
import Pomo.Data.AssetUrls (AssetUrls)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: AssetUrls -> Effect Unit
main assetUrls = HA.runHalogenAff do
  body <- HA.awaitBody
  let env = { runEnv: RunEnv.Dev, assetUrls }
      rootComponent = H.hoist (runAppM env) Router.component

  halogenIO <- runUI rootComponent {} body

  void $ H.liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

  pure unit
