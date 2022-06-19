{ name = "pointed-list"
, dependencies =
  [ "arrays"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "test-unit"
  ]
, license = "BSD-3-Clause"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, repository = "https://github.com/paluh/pointed-list.git"
}
