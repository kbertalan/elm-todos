module Pages.NotFound exposing (Model, Msg, Params, page)

import Element exposing (..)
import Page as Page exposing (Page)
import Request exposing (Request)
import Shared
import View exposing (View)


type alias Params =
    ()


type alias Model =
    ()


type alias Msg =
    Never


page : Shared.Model -> Request -> Page
page shared req =
    Page.static
        { view = view
        }



-- VIEW


view : View Msg
view =
    { title = "404"
    , element =
        text "Page not found"
    }
