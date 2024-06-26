{ name = "eth-core"
, dependencies =
  [ "argonaut"
  , "arrays"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "gen"
  , "integers"
  , "js-bigints"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "nonempty"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "simple-json"
  , "strings"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
