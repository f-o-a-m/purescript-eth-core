{ name = "eth-core"
, dependencies =
  [ "argonaut"
  , "bytestrings"
  , "console"
  , "debug"
  , "effect"
  , "foreign-generic"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "ring-modules"
  , "simple-json"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
