module Pages.Top exposing (Model, Msg, Params, page)

import Element exposing (..)
import Element.Border as Border exposing (..)
import Element.Font as Font exposing (..)
import Element.Input as Input exposing (button)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Widget exposing (button)


type alias Params =
    ()


type alias Model =
    { counter : Int }


type Msg
    = Increment
    | Decrement


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
    { counter = 0 }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 }

        Decrement ->
            { model | counter = model.counter - 1 }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Counter page"
    , body =
        [ row [ padding 10, spacing 5 ]
            [ Widget.button black Decrement "-"
            , el [ Font.bold ]
                (text (String.fromInt model.counter))
            , Widget.button black Increment "+"
            ]
        ]
    }


black : Color
black =
    rgb255 0 0 0
