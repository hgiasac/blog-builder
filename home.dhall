let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "src/home/**/*.purs" ]
}
