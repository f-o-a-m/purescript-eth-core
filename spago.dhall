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
  , "parsing"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quotient"
  , "ring-modules"
  , "simple-json"
  , "strings"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
