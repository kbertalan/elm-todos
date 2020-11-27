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
    , updating : Maybe Todo.Model
    }


type Msg
    = Edited String
    | GotTodos (Api.Data (List Todo.Model))
    | GotTodoEdit Todo.Model String
    | GotTodoEditReset Todo.Model
    | GotTodoChangeSubmitted Todo.Model String
    | TodoUpdated (Api.Data Todo.Model)
    | NoOp


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
      , updating = Nothing
      }
    , loadTodos
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edited desc ->
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

        GotTodoEdit todo change ->
            let
                newTodo =
                    { todo | change = Just change }

                newModel =
                    { model | todos = replace newTodo model.todos }
            in
            ( newModel, Todo.focus NoOp newTodo )

        GotTodoEditReset todo ->
            let
                newTodo =
                    { todo | change = Nothing }

                newModel =
                    { model | todos = replace newTodo model.todos }
            in
            ( newModel, Cmd.none )

        GotTodoChangeSubmitted todo change ->
            let
                newTodo =
                    { todo | description = change, change = Nothing }

                newModel =
                    { model | todos = replace newTodo model.todos, updating = Just todo }
            in
            ( newModel, updateTodo newTodo )

        TodoUpdated data ->
            case data of
                Api.NotAsked ->
                    ( model, Cmd.none )

                Api.Loading ->
                    ( model, Cmd.none )

                Api.SlowLoading ->
                    ( model, Cmd.none )

                Api.Loaded todo ->
                    let
                        newModel =
                            { model | updating = Nothing }
                    in
                    ( newModel, loadTodos )

                Api.Failed _ ->
                    case model.updating of
                        Nothing ->
                            ( model, Cmd.none )

                        Just old ->
                            let
                                newModel =
                                    { model | todos = replace old model.todos, updating = Nothing }
                            in
                            ( newModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


replace : Todo.Model -> Api.Data (List Todo.Model) -> Api.Data (List Todo.Model)
replace newTodo =
    Api.map <|
        List.map
            (\todo ->
                if todo.id == newTodo.id then
                    newTodo

                else
                    todo
            )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Todos"
    , body =
        [ column [ padding 10, spacing 5, centerX, Element.width (px 400) ] <|
            [ el [ Font.center, Element.width fill, Font.size 80, Font.color Color.white ] (text "Todo")
            , Todo.creatorView model.description { onEdit = Edited }
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
            List.map
                (Todo.view
                    { onChange = GotTodoEdit
                    , onIgnoreChange = GotTodoEditReset
                    , onSubmitChange = GotTodoChangeSubmitted
                    }
                )
                result

        Api.Failed error ->
            [ text "Failed" ]


loadTodos : Cmd Msg
loadTodos =
    Http.get
        { url = "/api/todo"
        , expect = Api.expectJson GotTodos (D.list Todo.decoder)
        }


updateTodo : Todo.Model -> Cmd Msg
updateTodo todo =
    Http.request
        { url = "/api/todo"
        , method = "PUT"
        , body = Http.jsonBody <| Todo.encoder todo
        , expect = Api.expectJson TodoUpdated Todo.decoder
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }
