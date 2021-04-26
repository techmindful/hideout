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
import Common.Urls exposing ( rootUrl )
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


footer : Element msg
footer =
    Element.row
        [ widthConstraint
        , Element.paddingXY 0 20
        , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
        ]
        [ link rootUrl "Hideout Home" ]

