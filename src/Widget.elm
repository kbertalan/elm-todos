module Widget exposing (..)

import Element exposing (..)
import Element.Border as Border exposing (..)
import Element.Input as Input


button : Color -> msg -> String -> Element msg
button color msg txt =
    Input.button
        [ Border.solid
        , Border.color color
        , Border.width 1
        , Border.rounded 2
        , Border.glow color 0.5
        ]
        { onPress = Just msg
        , label =
            el
                [ padding 5
                , Element.width fill
                ]
            <|
                text txt
        }
