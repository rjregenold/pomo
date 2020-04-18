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
  , "console"
  , "effect"
  , "formatters"
  , "generics-rep"
  , "halogen"
  , "halogen-hooks"
  , "now"
  , "numbers"
  , "psci-support"
  , "type-equality"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
