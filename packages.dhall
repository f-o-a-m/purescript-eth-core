let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240416/packages.dhall
        sha256:ca727657c01cc31d0e79c2113b59126b9826f4b56d20a8193be3c725599fb754


let additions =
      { bytestrings =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "integers"
          , "leibniz"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "quotient"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/rightfold/purescript-bytestrings"
        , version = "6733a32fca306015b3428e9985ffac65325a9864"
        }
      , quotient =
        { dependencies = [ "prelude", "quickcheck" ]
        , repo = "https://github.com/rightfold/purescript-quotient.git"
        , version = "v3.0.0"
        }
      }

in  upstream // additions
