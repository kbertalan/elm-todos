module Pages.Top exposing (Model, Msg, Params, page)

import Color
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as D
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


type alias Params =
    ()


type alias Todo =
    { id : String
    , description : String
    , completed : Bool
    }


type alias Model =
    { description : String
    , todos : List Todo
    }


type Msg
    = Edit String
    | Create
    | Loaded (Result Http.Error (List Todo))


page : Page Params Model Msg
page =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


init : Url Params -> ( Model, Cmd Msg )
init _ =
    ( { description = ""
      , todos = []
      }
    , load
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edit desc ->
            let
                newModel =
                    { model | description = desc }
            in
            ( newModel, Cmd.none )

        Create ->
            ( model, Cmd.none )

        Loaded result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model | todos = data }
                    in
                    ( newModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Todos"
    , body =
        [ column [ padding 10, spacing 5, centerX, centerY ]
            ([ Input.text
                []
                { label = Input.labelHidden "current"
                , onChange = Edit
                , placeholder = Just <| Input.placeholder [] (text "todo")
                , text = model.description
                }
             ]
                ++ List.map
                    (\todo ->
                        text todo.description
                    )
                    model.todos
            )
        ]
    }



-- CMD


load : Cmd Msg
load =
    Http.get
        { url = "/api/todo"
        , expect = Http.expectJson Loaded (D.list todoDecoder)
        }


todoDecoder : D.Decoder Todo
todoDecoder =
    D.map3 Todo
        (D.field "id" D.string)
        (D.field "description" D.string)
        (D.field "completed" D.bool)
