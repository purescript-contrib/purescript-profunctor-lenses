let config = ./spago.dhall
in config //
  { dependencies =
      config.dependencies #
        [ "colors"
        , "generics-rep"
        , "record-extra"
        ]
  , sources =
      config.sources #
        [ "examples/**/*.purs"
        ]
  }
