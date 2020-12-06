module Pages.Top exposing (Model, Msg, Params, page)

import Api exposing (Api)
import Color
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Http
import Json.Decode as D
import Json.Encode as E
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Todo


type alias Params =
    ()


type alias Todos =
    Api (Dict String (Api Todo.Model))


type alias Model =
    { description : String
    , todos : Todos
    }


type Msg
    = Edited String
    | Submitted String
    | TodoUpdated (Api Todo.Model)
    | TodoSaved (Api Todo.Model)
    | GotTodos (Api (List Todo.Model))
    | GotTodoSwitchedToEdit Todo.Model
    | GotTodoEdit Todo.Model String
    | GotTodoEditReset Todo.Model
    | GotTodoChangeSubmitted Todo.Model String
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
      , todos = Api.Loading Nothing
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

        Submitted desc ->
            ( model, createTodo desc )

        TodoUpdated data ->
            case data of
                Api.NotAsked ->
                    ( model, Cmd.none )

                Api.Loading _ ->
                    ( model, Cmd.none )

                Api.SlowLoading _ ->
                    ( model, Cmd.none )

                Api.Loaded todo ->
                    let
                        newModel =
                            { model | todos = Api.loading model.todos }
                    in
                    ( newModel, loadTodos )

                Api.Failed _ _ ->
                    ( model, Cmd.none )

        TodoSaved data ->
            case data of
                Api.NotAsked ->
                    ( model, Cmd.none )

                Api.Loading _ ->
                    ( model, Cmd.none )

                Api.SlowLoading _ ->
                    ( model, Cmd.none )

                Api.Loaded todo ->
                    let
                        newModel =
                            { model | description = "" }
                    in
                    ( newModel, loadTodos )

                Api.Failed _ _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        GotTodos data ->
            let
                todoMapper =
                    Api.map
                        (\list ->
                            list
                                |> List.map (\todo -> ( todo.id, Api.Loaded todo ))
                                |> Dict.fromList
                        )

                newModel =
                    { model | todos = Api.switch model.todos <| todoMapper data }
            in
            ( newModel, Cmd.none )

        GotTodoSwitchedToEdit todo ->
            let
                newTodo =
                    { todo | change = Just todo.description }

                newModel =
                    { model | todos = replace Api.Loaded newTodo model.todos }
            in
            ( newModel, Todo.focus NoOp newTodo )

        GotTodoEdit todo change ->
            let
                newTodo =
                    { todo | change = Just change }

                newModel =
                    { model | todos = replace Api.Loaded newTodo model.todos }
            in
            ( newModel, Cmd.none )

        GotTodoEditReset todo ->
            let
                newTodo =
                    { todo | change = Nothing }

                newModel =
                    { model | todos = replace Api.Loaded newTodo model.todos }
            in
            ( newModel, Cmd.none )

        GotTodoChangeSubmitted todo change ->
            let
                newTodo =
                    { todo | description = change, change = Nothing }

                newModel =
                    { model | todos = replace (Api.Loading << Just) todo model.todos }
            in
            ( newModel, updateTodo newTodo todo )


replace : (Todo.Model -> Api Todo.Model) -> Todo.Model -> Todos -> Todos
replace api newTodo =
    api newTodo
        |> Dict.insert newTodo.id
        |> Api.map



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Todos"
    , body =
        [ column [ padding 10, spacing 5, centerX, Element.width (px 400) ] <|
            [ el [ Font.center, Element.width fill, Font.size 80, Font.color Color.primary ] (text "Todo")
            , Todo.creatorView model.description { onEdit = Edited, onSubmit = Submitted }
            , column [ spacing 4, Element.width fill ]
                (apiTodosView model.todos)
            ]
        ]
    }


apiTodosView : Todos -> List (Element Msg)
apiTodosView todos =
    case todos of
        Api.NotAsked ->
            []

        Api.Loading prev ->
            case prev of
                Nothing ->
                    []

                Just p ->
                    apiTodosView <| Api.Loaded p

        Api.SlowLoading _ ->
            [ row
                [ Font.color Color.warning
                , centerX
                , centerY
                , spacing 10
                , padding 10
                ]
                [ FeatherIcons.loader |> FeatherIcons.toHtml [] |> html
                , text "Loading..."
                ]
            ]

        Api.Loaded result ->
            Dict.values result
                |> List.map apiTodoView

        Api.Failed error _ ->
            [ text "Failed" ]


apiTodoView : Api Todo.Model -> Element Msg
apiTodoView result =
    case result of
        Api.NotAsked ->
            Element.none

        Api.Loading _ ->
            Element.none

        Api.SlowLoading _ ->
            Element.none

        Api.Loaded data ->
            Todo.view
                { onStartChange = GotTodoSwitchedToEdit
                , onChange = GotTodoEdit
                , onIgnoreChange = GotTodoEditReset
                , onSubmitChange = GotTodoChangeSubmitted
                }
                data

        Api.Failed _ _ ->
            Element.none


loadTodos : Cmd Msg
loadTodos =
    Cmd.batch
        [ Http.get
            { url = "/api/todo"
            , expect = Api.expectJson GotTodos Nothing (D.list Todo.decoder)
            }
        , Api.delay 500 <| GotTodos <| Api.SlowLoading Nothing
        ]


updateTodo : Todo.Model -> Todo.Model -> Cmd Msg
updateTodo todo prev =
    Http.request
        { url = "/api/todo/" ++ todo.id
        , method = "PUT"
        , body = Http.jsonBody <| Todo.encoder todo
        , expect = Api.expectJson TodoUpdated (Just prev) Todo.decoder
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }


createTodo : String -> Cmd Msg
createTodo description =
    Http.post
        { url = "/api/todo"
        , body = Http.jsonBody <| E.object [ ( "description", E.string description ) ]
        , expect = Api.expectJson TodoSaved Nothing Todo.decoder
        }
