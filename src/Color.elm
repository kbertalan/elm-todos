module Color exposing (..)

import Element exposing (Color, fromRgb, rgb255, toRgb)


grey : Color
grey =
    rgb255 210 210 210


lightGrey : Color
lightGrey =
    rgb255 235 235 235


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


background : Color
background =
    rgb255 249 237 105
        |> scale 2


secondary : Color
secondary =
    rgb255 240 138 93
        |> scale 0.5


primary : Color
primary =
    rgb255 184 59 94


warning : Color
warning =
    rgb255 106 44 112


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
