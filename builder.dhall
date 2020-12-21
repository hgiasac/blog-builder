let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "src/builder/**/*.purs" ]
}
