{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "cirru-edn"
  , "cirru-parser"
  , "console"
  , "debug"
  , "effect"
  , "exceptions"
  , "integers"
  , "node-child-process"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "psci-support"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
