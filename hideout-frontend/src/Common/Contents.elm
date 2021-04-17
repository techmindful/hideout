module Common.Contents exposing (..)

import Element exposing ( Element )
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Common.Colors exposing ( red )
import Utils.Types exposing ( PosIntInput(..) )


plainPara str = Element.paragraph [] [ Element.text str ]


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



posIntInputHint : PosIntInput -> Element msg
posIntInputHint input =
    case input of
        Good _ -> Element.none
        Bad  _ ->
            Element.el
                [ Font.color red ] <|
                Element.text "Please input a positive integer."

