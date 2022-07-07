let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220630/packages.dhall sha256:691aff166010760f18ab1f4842ba6184f43747756e00579a050a2a46fa22d014

let additions = 
  { bytestrings =
    { dependencies =
      [ "arrays"
      , "effect"
      , "exceptions"
      , "foldable-traversable"
      , "integers"
      , "leibniz"
      , "maybe"
      , "newtype"
      , "node-buffer"
      , "prelude"
      , "quickcheck"
      , "quotient"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/martyall/purescript-bytestrings.git"
    , version = "upgrade-spago-purs-v0.15"
    },
  foreign-generic =
    { dependencies =
      [ "effect"
      , "exceptions"
      , "foreign"
      , "foreign-object"
      , "identity"
      , "ordered-collections"
      , "record"
      ]
    , repo = "https://github.com/jsparkes/purescript-foreign-generic"
    , version = "v0.15"
    }
  }

in  upstream // additions
