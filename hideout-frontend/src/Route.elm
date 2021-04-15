module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type Route
    = Root
    | About
    | ReadLetter String
    | WriteLetter
    | Chat String
    | ConfigChat
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Root top
        , map About <| s "about"
        , map ReadLetter <| s "read-letter" </> string
        , map WriteLetter <| s "write-letter"
        , map Chat <| s "chat" </> string
        , map ConfigChat <| s "config-chat"
        ]


getRoute : Url -> Route
getRoute url =
    Maybe.withDefault NotFound <| parse routeParser url
