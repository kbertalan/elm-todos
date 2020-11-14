module Pages.Top exposing (Model, Msg, Params, page)

import Color
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


type alias Params =
    ()


type alias Model =
    { current : String }


type Msg
    = ChangeText String


page : Page Params Model Msg
page =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


init : Url Params -> Model
init _ =
    { current = "" }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeText txt ->
            { model | current = txt }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Todos"
    , body =
        [ column [ padding 10, spacing 5, centerX, centerY ]
            [ Input.text []
                { label = Input.labelHidden "current"
                , onChange = ChangeText
                , placeholder = Just <| Input.placeholder [] (text "todo")
                , text = model.current
                }
            ]
        ]
    }
