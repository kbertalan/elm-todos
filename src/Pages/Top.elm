module Pages.Top exposing (Model, Msg, Params, page)

import Api
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
import Todo


type alias Params =
    ()


type alias Model =
    { description : String
    , todos : Api.Data (List Todo.Model)
    }


type Msg
    = Edit String
    | GotTodos (Api.Data (List Todo.Model))


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
      , todos = Api.Loading
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

        GotTodos data ->
            let
                newModel =
                    { model | todos = data }
            in
            ( newModel, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Todos"
    , body =
        [ column [ padding 10, spacing 5, centerX, Element.width (px 400) ] <|
            [ el [ Font.center, Element.width fill, Font.size 80, Font.color Color.white ] (text "Todo")
            , Todo.creatorView model.description { onEdit = Edit }
            , column [ spacing 2, Element.width fill ]
                (apiTodoView model.todos)
            ]
        ]
    }


apiTodoView : Api.Data (List Todo.Model) -> List (Element Msg)
apiTodoView todos =
    case todos of
        Api.NotAsked ->
            []

        Api.Loading ->
            [ text "Loading..." ]

        Api.SlowLoading ->
            [ text "Loading..." ]

        Api.Loaded result ->
            List.map Todo.view result

        Api.Failed error ->
            [ text "Failed" ]


load : Cmd Msg
load =
    Http.get
        { url = "/api/todo"
        , expect = Api.expectJson GotTodos (D.list Todo.decoder)
        }
