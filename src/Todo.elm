module Todo exposing (..)

import Browser.Dom
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html.Attributes
import Json.Decode as D
import Json.Encode as E
import Keys
import Task


type Completed
    = Done
    | ToBeDone


type Change
    = None
    | Content String
    | DeleteTriggered


type alias Model =
    { id : String
    , description : String
    , completed : Completed
    , change : Change
    }


creatorView :
    String
    ->
        { onEdit : String -> msg
        , onSubmit : String -> msg
        }
    -> Element msg
creatorView description msgs =
    Input.text
        [ Element.width fill
        , Background.color Color.background
        , Input.focusedOnLoad
        , Font.color Color.highlight
        , Border.color Color.highlight
        , Element.focused
            [ Border.color Color.highlight
            , Border.glow Color.highlight 2
            ]
        , Keys.onKeyUp
            [ Keys.enter (msgs.onSubmit description)
            ]
        ]
        { label = Input.labelHidden "new todo"
        , onChange = msgs.onEdit
        , placeholder =
            Just <|
                Input.placeholder
                    [ Font.color <| Color.scale 0.5 Color.highlight ]
                    (text "Add new todo item here")
        , text = description
        }


view :
    { onStartChange : Model -> msg
    , onChange : Model -> String -> msg
    , onIgnoreChange : Model -> msg
    , onSubmitChange : Model -> String -> msg
    , onDeleteTriggered : Model -> msg
    , onDeleteCancelled : Model -> msg
    , onDeleteConfirmed : Model -> msg
    }
    -> Model
    -> Element msg
view msgs model =
    case model.change of
        None ->
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
                [ el
                    [ width fill
                    ]
                    (text model.description)
                , el
                    [ Element.alignRight
                    , Element.pointer
                    , Events.onClick (msgs.onStartChange model)
                    , Font.color Color.primary
                    ]
                    (FeatherIcons.edit |> FeatherIcons.toHtml [] |> html)
                ]

        Content change ->
            row
                [ padding 10
                , spacing 10
                , Element.width fill
                , Background.color <| Color.scale 1.5 Color.background
                , Border.solid
                , Border.width 1
                , Border.color Color.secondary
                , Border.glow Color.secondary 2
                , Element.focused
                    [ Border.color Color.primary
                    , Border.glow Color.primary 2
                    ]
                , Font.color Color.primary
                ]
                [ Input.text
                    [ padding 0
                    , Element.htmlAttribute <| Html.Attributes.id model.id
                    , Background.color <| Color.scale 1.5 Color.background
                    , Border.width 0
                    , Border.glow Color.background 0
                    , Border.rounded 0
                    , Element.focused
                        [ Border.glow Color.background 0
                        ]
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
                , el
                    [ Element.alignRight
                    , Element.pointer
                    , Events.onClick (msgs.onSubmitChange model change)
                    , Font.color Color.primary
                    ]
                    (FeatherIcons.save |> FeatherIcons.toHtml [] |> html)
                , el
                    [ Element.alignRight
                    , Element.pointer
                    , Events.onClick (msgs.onDeleteTriggered model)
                    , Font.color Color.warning
                    ]
                    (FeatherIcons.trash |> FeatherIcons.toHtml [] |> html)
                , el
                    [ Element.alignRight
                    , Element.pointer
                    , Events.onClick (msgs.onIgnoreChange model)
                    , Font.color Color.secondary
                    ]
                    (FeatherIcons.xSquare |> FeatherIcons.toHtml [] |> html)
                ]

        DeleteTriggered ->
            row
                [ padding 10
                , spacing 10
                , Element.width fill
                , Background.color <| Color.scale 1.5 Color.background
                , Border.solid
                , Border.width 1
                , Border.color Color.secondary
                , Border.glow Color.secondary 2
                , Element.focused
                    [ Border.color Color.primary
                    , Border.glow Color.primary 2
                    ]
                , Font.color Color.warning
                ]
                [ el
                    [ Element.alignLeft
                    , Element.pointer
                    , Events.onClick (msgs.onDeleteConfirmed model)
                    , Font.color Color.primary
                    ]
                    (FeatherIcons.checkSquare |> FeatherIcons.toHtml [] |> html)
                , el
                    [ width fill
                    ]
                    (text "Are you sure to delete this item?")
                , el
                    [ Element.alignRight
                    , Element.pointer
                    , Events.onClick (msgs.onDeleteCancelled model)
                    , Font.color Color.secondary
                    ]
                    (FeatherIcons.xSquare |> FeatherIcons.toHtml [] |> html)
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
        (D.succeed None)


focus : msg -> Model -> Cmd msg
focus msg model =
    Task.attempt (\_ -> msg) <| Browser.Dom.focus model.id
