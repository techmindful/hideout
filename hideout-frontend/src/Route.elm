module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, fragment, map, oneOf, parse, s, string, top)
import Views.About


type Route
    = Root
    | About Views.About.Section
    | ReadLetter String
    | WriteLetter
    | ConfigChat
    | Entrance String
    | Chat String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Root top
        , map About <| s "about" </> fragment Views.About.urlFragToSection
        , map ReadLetter <| s "read-letter" </> string
        , map WriteLetter <| s "write-letter"
        , map ConfigChat <| s "config-chat"
        , map Entrance <| s "persist-chat-entrance" </> string
        , map Chat <| s "chat" </> string
        ]


getRoute : Url -> Route
getRoute url =
    Maybe.withDefault NotFound <| parse routeParser url
