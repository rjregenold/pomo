{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "argonaut-generic"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "formatters"
  , "generics-rep"
  , "halogen"
  , "now"
  , "numbers"
  , "psci-support"
  , "type-equality"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
