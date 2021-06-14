module Common.Contents exposing (..)

import Element exposing ( Element )
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Common.Colors exposing (..)
import Common.Styles exposing
    ( linkStyle
    , widthConstraint
    )
import Common.Urls exposing ( rootUrl, aboutUrl )
import Time exposing ( Posix )
import Utils.Types exposing ( PosIntInput(..), strToPosIntInput )
import Utils.Utils as Utils


plainPara str = Element.paragraph [] [ Element.text str ]


link : String -> String -> Element msg
link urlStr labelStr =
    Element.link
        linkStyle
        { url = urlStr
        , label = Element.text labelStr
        }


newTabLink : String -> String -> Element msg
newTabLink urlStr labelStr =
    Element.newTabLink
        linkStyle
        { url = urlStr
        , label = Element.text labelStr
        }


italicText : String -> Element msg
italicText str =
    Element.el
        [ Font.italic ]
        ( Element.text str )


underlinedText : String -> Element msg
underlinedText str =
    Element.el
        [ Font.underline ]
        ( Element.text str )


sizedText : Int -> String -> Element msg
sizedText fontSize str =
    Element.el
        [ Font.size fontSize ]
        ( Element.text str )


sizedPara : Int -> String -> Element msg
sizedPara fontSize str =
    Element.paragraph
        [ Font.size fontSize ]
        [ Element.text str ]


spacedPara : Int -> String -> Element msg
spacedPara lineSpacing str =
    Element.paragraph
        [ Element.spacing lineSpacing ]
        [ Element.text str ]


borderedButton : m -> String -> Element m
borderedButton msg labelStr =
    Input.button
        [ Element.padding 5
        , Border.width 2
        , Border.rounded 6
        ]
        { onPress = Just msg
        , label = Element.text labelStr
        }


posIntInputHint : String -> Element msg
posIntInputHint input =
    case strToPosIntInput input of
        Good _ -> Element.none
        Bad  _ ->
            Element.el
                [ Font.color red ] <|
                Element.text "Please input a positive integer."


timeText : Posix -> Posix -> Element m
timeText targetPosix currentPosix =
    Element.el
        [ Font.size 16
        , Font.color grey
        ] <|
        Element.text <|
            Utils.formatTime targetPosix currentPosix


rootLink : Element msg
rootLink = link rootUrl "Hideout Home"


type Tabness
    = SameTab
    | NewTab


linkForTabness tabness =
    case tabness of
        SameTab -> Element.link
        NewTab  -> Element.newTabLink


footer : Int -> Tabness -> Element msg
footer topPadding tabness =
    Element.el
        [ Element.paddingEach { top = topPadding, bottom = 0, left = 0, right = 0 }
        , Element.width Element.fill
        ] <|
        Element.column
            [ widthConstraint
            , Element.paddingXY 0 20
            , Element.spacingXY 0 10
            , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
            ]
            [ ( linkForTabness tabness )
                []
                { url = aboutUrl
                , label = Element.text "What is Hideout?"
                }
            , ( linkForTabness tabness )
                []
                { url = rootUrl
                , label = Element.text "Hideout Home"
                }
            ]



