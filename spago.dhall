{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "calcit-pure-run"
, dependencies =
  [ "aff"
  , "argparse-basic"
  , "arrays"
  , "cirru-edn"
  , "cirru-parser"
  , "console"
  -- , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "node-child-process"
  , "node-fs-aff"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "numbers"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "psci-support"
  , "refs"
  , "strings"
  , "tuples"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
