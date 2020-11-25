module Keys exposing (Key(..), enter, escape, onKeyUp)

import Element
import Html.Events exposing (on)
import Json.Decode as D


type Key
    = Enter
    | Escape


type alias Matcher msg =
    Key -> Maybe msg


enter : msg -> Matcher msg
enter msg =
    \key ->
        case key of
            Enter ->
                Just msg

            _ ->
                Nothing


escape : msg -> Matcher msg
escape msg =
    \key ->
        case key of
            Escape ->
                Just msg

            _ ->
                Nothing


onKeyUp : List (Key -> Maybe msg) -> Element.Attribute msg
onKeyUp keyFns =
    Element.htmlAttribute <|
        on "keyup"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        case fromString key of
                            Just enteredKey ->
                                List.filterMap (\fn -> fn enteredKey) keyFns
                                    |> List.reverse
                                    |> List.head
                                    |> Maybe.map D.succeed
                                    |> Maybe.withDefault (D.fail "Not configured key")

                            Nothing ->
                                D.fail "Not recognized"
                    )
            )


matcher : Key -> String -> Bool
matcher expectedKey enteredKey =
    let
        maybeKey =
            fromString enteredKey
    in
    case maybeKey of
        Just key ->
            expectedKey == key

        Nothing ->
            False


fromString : String -> Maybe Key
fromString key =
    case key of
        "Enter" ->
            Just Enter

        "Escape" ->
            Just Escape

        _ ->
            Nothing


toString : Key -> String
toString key =
    case key of
        Enter ->
            "Enter"

        Escape ->
            "Escape"
