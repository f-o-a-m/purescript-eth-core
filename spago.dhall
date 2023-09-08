{ name = "eth-core"
, dependencies =
  [ "argonaut"
  , "arrays"
  , "bytestrings"
  , "console"
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
  , "unsafe-coerce"
  , "js-bigints"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
