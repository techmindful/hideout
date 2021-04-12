module Utils.Markdown exposing (..)


import Html exposing ( Html )
import List
import Parser exposing ( DeadEnd, Problem )
import Markdown.Parser exposing ( deadEndToString, parse )
import Markdown.Renderer exposing ( defaultHtmlRenderer )


render : String -> Result String ( List ( Html msg ) )
render str =
    str |> parse
        |> Result.mapError deadEndsToString
        |> Result.andThen ( \ blocks -> Markdown.Renderer.render defaultHtmlRenderer blocks )


deadEndsToString deadEnds =
    deadEnds |> List.map deadEndToString |> String.join "\n"

