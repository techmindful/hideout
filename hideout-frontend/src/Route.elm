module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, fragment, map, oneOf, parse, s, string, top)
import Views.About


type Route
    = Root
    | About ( Views.About.Section )
    | ReadLetter String
    | WriteLetter
    | Chat String
    | ConfigChat
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Root top
        , map About <| s "about" </> fragment Views.About.urlFragToSection
        , map ReadLetter <| s "read-letter" </> string
        , map WriteLetter <| s "write-letter"
        , map Chat <| s "chat" </> string
        , map ConfigChat <| s "config-chat"
        ]


getRoute : Url -> Route
getRoute url =
    Maybe.withDefault NotFound <| parse routeParser url
