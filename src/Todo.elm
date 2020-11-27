module Todo exposing (..)

import Browser.Dom
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Json.Decode as D
import Json.Encode as E
import Keys
import Task


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
        [ Element.width fill
        , Background.color Color.background
        , Input.focusedOnLoad
        , Font.color Color.primary
        , Border.color Color.secondary
        , Element.focused
            [ Border.color Color.secondary
            , Border.glow Color.secondary 2
            ]
        ]
        { label = Input.labelHidden "current"
        , onChange = msgs.onEdit
        , placeholder =
            Just <|
                Input.placeholder
                    [ Font.color Color.secondary
                    ]
                    (text "Add new todo item here")
        , text = description
        }


view :
    { onStartChange : Model -> msg
    , onChange : Model -> String -> msg
    , onIgnoreChange : Model -> msg
    , onSubmitChange : Model -> String -> msg
    }
    -> Model
    -> Element msg
view msgs model =
    case model.change of
        Nothing ->
            row
                [ spacing 10
                , padding 10
                , Border.solid
                , Border.width 1
                , Element.width fill
                , Border.rounded 2
                , Events.onDoubleClick (msgs.onStartChange model)
                , Font.color Color.secondary
                , Border.color Color.secondary
                ]
                [ text model.description ]

        Just change ->
            Input.text
                [ Element.htmlAttribute <| Html.Attributes.id model.id
                , Element.width fill
                , Element.height fill
                , Background.color <| Color.scale 1.1 Color.background
                , Border.color Color.secondary
                , Border.glow Color.secondary 2
                , Element.focused
                    [ Border.color Color.secondary
                    , Border.glow Color.secondary 2
                    ]
                , Font.color Color.primary
                , Keys.onKeyUp
                    [ Keys.enter (msgs.onSubmitChange model change)
                    , Keys.escape (msgs.onIgnoreChange model)
                    ]
                ]
                { label = Input.labelHidden ""
                , onChange = msgs.onChange model
                , placeholder = Nothing
                , text = change
                }


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


completedToBool : Completed -> Bool
completedToBool completed =
    case completed of
        Done ->
            True

        ToBeDone ->
            False


encoder : Model -> E.Value
encoder model =
    E.object
        [ ( "id", E.string model.id )
        , ( "description", E.string model.description )
        , ( "completed", E.bool <| completedToBool model.completed )
        ]


decoder : D.Decoder Model
decoder =
    D.map4 Model
        (D.field "id" D.string)
        (D.field "description" D.string)
        (D.field "completed" <| D.map completedFromBool D.bool)
        (D.succeed Nothing)


focus : msg -> Model -> Cmd msg
focus msg model =
    Task.attempt (\_ -> msg) <| Browser.Dom.focus model.id
