{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "cirru-edn"
  , "cirru-parser"
  , "console"
  , "effect"
  , "exceptions"
  , "node-fs-aff"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
