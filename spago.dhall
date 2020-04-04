{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "formatters"
  , "halogen"
  , "now"
  , "numbers"
  , "psci-support"
  , "type-equality"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
