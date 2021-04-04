{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "cirru-edn"
  , "cirru-parser"
  , "console"
  , "debug"
  , "effect"
  , "exceptions"
  , "integers"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "psci-support"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
