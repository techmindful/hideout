module Utils.Markdown exposing (..)


import Element exposing ( Element )
import Html exposing ( Html )
import List
import Parser exposing ( DeadEnd, Problem )
import Markdown.Parser exposing ( deadEndToString, parse )
import Markdown.Renderer exposing ( defaultHtmlRenderer )


render : String -> Element msg
render str =
    let result = str |> parse
                     |> Result.mapError deadEndsToString
                     |> Result.andThen ( \ blocks -> Markdown.Renderer.render defaultHtmlRenderer blocks )
    in
    Element.html <|
        Html.div [] <|
            case result of
                Err errStr -> [ Html.text errStr ]
                Ok views -> views


deadEndsToString deadEnds =
    deadEnds |> List.map deadEndToString |> String.join "\n"

