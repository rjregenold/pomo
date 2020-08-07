{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pomo"
, dependencies =
  [ "argonaut"
  , "argonaut-generic"
  , "canvas"
  , "codec-argonaut"
  , "colors"
  , "console"
  , "effect"
  , "formatters"
  , "generics-rep"
  , "halogen"
  , "halogen-formless"
  , "halogen-hooks"
  , "now"
  , "numbers"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "type-equality"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
