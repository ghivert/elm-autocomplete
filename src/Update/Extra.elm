module Update.Extra exposing (..)

identity : model -> (model, Cmd msg)
identity model =
  model ! []
