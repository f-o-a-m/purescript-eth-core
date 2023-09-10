{ name = "eth-core"
, dependencies =
  [ "argonaut"
  , "arrays"
  , "bytestrings"
  , "effect"
  , "either"
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
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
