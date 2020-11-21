module Todo exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Json.Decode as D


type Completed
    = Done
    | ToBeDone


type alias Model =
    { id : String
    , description : String
    , completed : Completed
    , change : Maybe String
    }


creatorView :
    String
    ->
        { onEdit : String -> msg
        }
    -> Element msg
creatorView description msgs =
    Input.text
        [ Element.width fill, Background.color Color.lightGrey ]
        { label = Input.labelHidden "current"
        , onChange = msgs.onEdit
        , placeholder = Just <| Input.placeholder [] (text "Add new todo item here")
        , text = description
        }


view : Model -> Element msg
view model =
    row [ spacing 10, padding 10, Border.solid, Border.width 1, Element.width fill, Border.rounded 2 ]
        [ text model.description
        , text <| completedAsString model.completed
        ]


completedAsString : Completed -> String
completedAsString completed =
    case completed of
        Done ->
            "Done"

        ToBeDone ->
            "To be done"


completedFromBool : Bool -> Completed
completedFromBool bool =
    case bool of
        True ->
            Done

        False ->
            ToBeDone


decoder : D.Decoder Model
decoder =
    D.map4 Model
        (D.field "id" D.string)
        (D.field "description" D.string)
        (D.field "completed" <| D.map completedFromBool D.bool)
        (D.succeed Nothing)
