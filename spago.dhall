{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "blog"
, dependencies =
  [ "canvas"
  , "console"
  , "drawing"
  , "effect"
  , "node-fs"
  , "now"
  , "prelude"
  , "psci-support"
  , "css"
  , "markdown-smolder"
  , "behaviors"
  , "canvas"
  , "drawing"
  , "web-html"
  , "now"
  , "precise"
  ]
, packages = ./packages.dhall
, sources = [ "src/common/**/*.purs" ]
}
