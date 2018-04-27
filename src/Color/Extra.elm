module Color.Extra exposing (..)

import Color exposing (Color)

toCssRgba : Color -> String
toCssRgba color =
  let { red, green, blue, alpha } = Color.toRgb color in
  [ red, green, blue ]
    |> List.map toString
    |> flip List.append [ toString alpha ]
    |> String.join ", "
    |> \content -> "rgba(" ++ content ++ ")"
