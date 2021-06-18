module Utils.Markdown exposing
    ( render
    , viewSpacing
    )

{-| Copied a lot from <https://ellie-app.com/bQLgjtbgdkZa1>
-}

import Common.Contents exposing (plainPara)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Emoji
import Html
import Html.Attributes
import List
import List.Extra as List
import Markdown.Block
    exposing
        ( Block
        , Inline
        , ListItem(..)
        , Task(..)
        , walkInlines
        )
import Markdown.Html
import Markdown.Parser exposing (deadEndToString, parse)
import Markdown.Renderer
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , backtrackable
        , chompIf
        , chompUntil
        , chompUntilEndOr
        , chompWhile
        , getChompedString
        , succeed
        , symbol
        )
import Regex exposing (Regex)
import String.Extra as String
import Tuple


render : String -> List (Element msg)
render str =
    let
        result =
            str
                |> Markdown.Parser.parse
                |> Result.map handleEmojis_Blocks
                |> Result.mapError
                    (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
                |> Result.andThen (Markdown.Renderer.render renderer)
    in
    case result of
        Err errStr ->
            [ plainPara errStr ]

        Ok rendered ->
            rendered


viewSpacing : Element.Attribute msg
viewSpacing =
    Element.spacingXY 0 15


handleEmojis_Blocks : List Block -> List Block
handleEmojis_Blocks =
    List.map <|
        \block ->
            case block of
                Markdown.Block.Paragraph inlines ->
                    Markdown.Block.Paragraph <| handleEmojis_Inlines inlines

                _ ->
                    block


handleEmojis_Inlines : List Inline -> List Inline
handleEmojis_Inlines inlines =
    inlines
        |> List.map
            (\inline ->
                case inline of
                    Markdown.Block.Text str ->
                        replaceEmojis str

                    _ ->
                        [ inline ]
            )
        |> List.concat


replaceEmojis : String -> List Inline
replaceEmojis str =
    let
        colonIndices : List Int
        colonIndices =
            String.indices ":" str

        firstColonPair : Maybe ( Int, Int )
        firstColonPair =
            Maybe.map2 Tuple.pair
                (List.getAt 0 colonIndices)
                (List.getAt 1 colonIndices)
    in
    case firstColonPair of
        -- No pair of colons. No emojis.
        Nothing ->
            [ Markdown.Block.Text str ]

        Just pair ->
            let
                firstColonIndex =
                    Tuple.first pair

                secondColonIndex =
                    Tuple.second pair

                possibleEmojiName =
                    String.slice (firstColonIndex + 1) secondColonIndex str

                isEmoji =
                    List.member possibleEmojiName Emoji.allHex
            in
            -- Has colon of pairs, but it doesn't match an emoji name.
            if not isEmoji then
                (Markdown.Block.Text <| String.left secondColonIndex str)
                    :: (replaceEmojis <| String.dropLeft secondColonIndex str)
                -- Found a emoji.

            else
                [ Markdown.Block.Text <| String.left firstColonIndex str
                , Markdown.Block.Image
                    (Emoji.hexToPath possibleEmojiName)
                    Nothing
                    []
                ]
                    ++ (replaceEmojis <| String.dropLeft (secondColonIndex + 1) str)



--emojisParser : Parser ( List Inline )
--emojisParser =
--    let
--        chompColon : Parser ()
--        chompColon = chompIf ( \c -> c == ':' )
--
--        chompUntilColon : Parser String
--        chompUntilColon = getChompedString <| chompUntil ":"
--
--        step : List Inline -> Parser ( Step ( List Inline ) ( List Inline ) )
--        step inlines =
--            let
--                plainTextParser : Parser ( Step ( List Inline ) ( List Inline ) )
--                plainTextParser =
--                    succeed
--                        ( \ newStr -> Loop <| inlines ++ [ Markdown.Block.Text newStr ] )
--                        |= chompUntilColon
--
--                emojiParser : Parser ( Step ( List Inline ) ( List Inline ) )
--                emojiParser =
--                    Parser.map
--                        ( \ emojiStr -> Loop <|
--                            inlines ++
--                            [ Markdown.Block.Image ( Emoji.hexToPath emojiStr ) Nothing [] ]
--                        )
--                        (  getChompedString <|
--                           symbol ":"
--                        |. chompUntil ":"
--                        |. symbol ":"
--                        )
--
--                endParser : Parser ( Step ( List Inline ) ( List Inline ) )
--                endParser =
--                    Parser.end |> Parser.map ( \_ -> Done inlines )
--            in
--            Parser.oneOf
--                [ emojiParser
--                , plainTextParser
--                , endParser
--                ]
--    in
--    Parser.loop [] step


renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacingXY 0 5 ]
    , thematicBreak = Element.none
    , text = \value -> Element.paragraph [] [ Element.text value ]
    , strong = \content -> Element.paragraph [ Font.bold ] content
    , emphasis = \content -> Element.paragraph [ Font.italic ] content
    , strikethrough = \content -> Element.paragraph [ Font.strike ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink []
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.underline
                        , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
                        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
                        ]
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            Element.image
                []
                { src = image.src
                , description = image.alt
                }
    , blockQuote =
        \children ->
            Element.paragraph
                [ Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Border.color (Element.rgb255 145 145 145)
                , Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 10 ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph
                                    [ Element.alignTop ]
                                    ((case task of
                                        IncompleteTask ->
                                            Input.defaultCheckbox False

                                        CompletedTask ->
                                            Input.defaultCheckbox True

                                        NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 10 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , table = Element.column []
    , tableHeader =
        Element.column
            [ Font.bold
            , Element.width Element.fill
            , Font.center
            ]
    , tableBody = Element.column []
    , tableRow = Element.row [ Element.height Element.fill, Element.width Element.fill ]
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , html =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "emoji"
                renderEmojiTag
                |> Markdown.Html.withAttribute "name"
            ]
    }


renderEmojiTag : String -> List (Element msg) -> Element msg
renderEmojiTag emojiName rendererdChildren =
    Element.image
        []
        { src = Emoji.hexToPath emojiName
        , description = ""
        }


alternateTableRowBackground =
    Element.rgb255 245 247 249


tableBorder =
    [ Border.color (Element.rgb255 223 226 229)
    , Border.width 1
    , Border.solid
    , Element.paddingXY 6 13
    , Element.height Element.fill
    ]


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


heading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                Markdown.Block.H1 ->
                    36

                Markdown.Block.H2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Font.family [ Font.typeface "Montserrat" ]
        , Region.heading (Markdown.Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


code : String -> Element msg
code snippet =
    Element.el
        [ Background.color
            (Element.rgba 0 0 0 0.04)
        , Border.rounded 2
        , Element.paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.paragraph
        [ Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
        , Element.padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        [ Element.text details.body ]
