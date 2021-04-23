module Utils.Errors exposing (..)

import Http


httpErrToStr : Http.Error -> String
httpErrToStr err =
    case err of
        Http.BadUrl str ->
            "Bad URL " ++ str

        Http.Timeout -> "Timeout"

        Http.NetworkError ->
            """
            Can't reach server. Either your network has a problem, or server is down.
            """

        Http.BadStatus code ->
            """
            Bad status. Error code: 
            """
            ++ String.fromInt code

        Http.BadBody str ->
            """
            Bad response body: 
            """
            ++ str

