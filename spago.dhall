{ name = "profunctor-lenses"
, dependencies =
  [ "arrays"
  , "assert"
  , "bifunctors"
  , "const"
  , "control"
  , "distributive"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "functors"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "record"
  , "safe-coerce"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
