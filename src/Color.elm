module Color exposing (..)

import Element exposing (Color, fromRgb, rgb255, toRgb)


background : Color
background =
    rgb255 39 39 39


secondary : Color
secondary =
    rgb255 116 116 116


primary : Color
primary =
    rgb255 20 167 108


highlight : Color
highlight =
    rgb255 255 228 0


warning : Color
warning =
    rgb255 255 101 47


scale : Float -> Color -> Color
scale multiplier color =
    toRgb color
        |> (\c ->
                { c
                    | red = c.red * multiplier |> max 0 |> min 1
                    , green = c.green * multiplier |> max 0 |> min 1
                    , blue = c.blue * multiplier |> max 0 |> min 1
                }
           )
        |> fromRgb
