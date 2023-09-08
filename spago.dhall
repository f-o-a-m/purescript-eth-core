{ name = "eth-core"
, dependencies =
  [ "argonaut"
  , "arrays"
  , "bytestrings"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "integers"
  , "js-bigints"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "quotient"
  , "ring-modules"
  , "simple-json"
  , "strings"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
