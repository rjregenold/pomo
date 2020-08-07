module Pomo.Component.HTML.Header where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

header :: forall i p. HH.HTML i p
header = 
  HH.header
    [ HP.classes $ HH.ClassName <$>
      [ "navbar" 
      , "flex-item-default"
      , "pt-2"
      ]
    ]
    [ HH.section 
      [ HP.class_ $ HH.ClassName "navbar-section" ]
      [ HH.a
        [ HP.href "#/"
        , HP.classes $ HH.ClassName <$> 
          [ "navbar-brand"
          , "text-bold"
          , "mr-2" 
          ]
        ]
        [ HH.text "Pomo" ]
      ]
    , HH.section
      [ HP.class_ $ HH.ClassName "navbar-section" ]
      [ HH.a
        [ HP.href "#/settings"
        , HP.classes $ HH.ClassName <$>
          [ "btn"
          ]
        ]
        [ HH.text "Settings" ]
      ]
    ]
