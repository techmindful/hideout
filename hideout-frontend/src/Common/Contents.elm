module Common.Contents exposing (..)

import Element exposing ( Element )
import Element.Font as Font
import Common.Colors exposing ( red )
import Utils.Types exposing ( PosIntInput(..) )


posIntInputHint : PosIntInput -> Element msg
posIntInputHint input =
    case input of
        Good _ -> Element.none
        Bad  _ ->
            Element.el
                [ Font.color red ] <|
                Element.text "Please input a positive integer."

