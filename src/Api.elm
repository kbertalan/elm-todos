module Api exposing (Api(..), delay, expectJson, expectWhatever, loading, map, switch)

import Http
import Json.Decode exposing (Decoder)
import Process
import Task


type Api a
    = NotAsked
    | Loading (Maybe a)
    | SlowLoading (Maybe a)
    | Loaded a
    | Failed Http.Error (Maybe a)


expectJson : (Api a -> msg) -> Maybe a -> Decoder a -> Http.Expect msg
expectJson toMsg prev decoder =
    Http.expectJson (fromResult prev >> toMsg) decoder


expectWhatever : (Api () -> msg) -> Http.Expect msg
expectWhatever toMsg =
    Http.expectWhatever (fromResult Nothing >> toMsg)


map : (a -> b) -> Api a -> Api b
map fn data =
    case data of
        NotAsked ->
            NotAsked

        Loading prev ->
            Loading <| Maybe.map fn prev

        SlowLoading prev ->
            SlowLoading <| Maybe.map fn prev

        Loaded a ->
            Loaded <| fn a

        Failed e prev ->
            Failed e <| Maybe.map fn prev


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


switch : Api a -> Api a -> Api a
switch current new =
    case new of
        NotAsked ->
            new

        Loading _ ->
            new

        SlowLoading _ ->
            case current of
                NotAsked ->
                    new

                Loading _ ->
                    new

                SlowLoading _ ->
                    new

                Loaded _ ->
                    current

                Failed _ _ ->
                    current

        Loaded _ ->
            new

        Failed _ _ ->
            new


loading : Api a -> Api a
loading api =
    case api of
        NotAsked ->
            Loading Nothing

        Loading _ ->
            api

        SlowLoading _ ->
            api

        Loaded v ->
            Loading <| Just v

        Failed _ p ->
            Loading p


fromResult : Maybe a -> Result Http.Error a -> Api a
fromResult prev result =
    case result of
        Ok value ->
            Loaded value

        Err error ->
            Failed error prev
