{ name = "eth-core"
, dependencies =
  [ "argonaut"
  , "arrays"
  , "bytestrings"
  , "effect"
  , "either"
  , "foreign"
  , "foreign-generic"
  , "functions"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "quotient"
  , "ring-modules"
  , "simple-json"
  , "strings"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
