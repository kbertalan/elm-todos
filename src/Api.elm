module Api exposing (Data(..), expectJson)

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


fromResult : Result Http.Error a -> Data a
fromResult result =
    case result of
        Ok value ->
            Loaded value

        Err error ->
            Failed error
