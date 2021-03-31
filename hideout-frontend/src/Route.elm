module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type Route
    = Root
    | About
    | ReadLetter String
    | WriteLetter
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Root top
        , map About <| s "about"
        , map ReadLetter <| s "read-letter" </> string
        , map WriteLetter <| s "write-letter"
        ]


getRoute : Url -> Route
getRoute url =
    Maybe.withDefault NotFound <| parse routeParser url
