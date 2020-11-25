module Api exposing (Data(..), expectJson, map)

import Http
import Json.Decode exposing (Decoder)


type Data a
    = NotAsked
    | Loading
    | SlowLoading
    | Loaded a
    | Failed Http.Error


expectJson : (Data a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectJson (fromResult >> toMsg) decoder


map : (a -> b) -> Data a -> Data b
map fn data =
    case data of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        SlowLoading ->
            SlowLoading

        Loaded a ->
            Loaded (fn a)

        Failed e ->
            Failed e


fromResult : Result Http.Error a -> Data a
fromResult result =
    case result of
        Ok value ->
            Loaded value

        Err error ->
            Failed error
