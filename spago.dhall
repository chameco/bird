{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "bird"
, dependencies =
  [ "console"
  , "effect"
  , "refs"
  , "psci-support"
  , "web-html"
  , "canvas"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
