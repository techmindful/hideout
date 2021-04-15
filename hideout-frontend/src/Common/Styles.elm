module Common.Styles exposing (..)

import Element
import Element.Border as Border
import Element.Font as Font


widthConstraint =
    Element.width <| Element.px 750


linkStyle =
    [ Font.underline ]


buttonStyle padding =
    [ Element.padding padding
    , Border.width 2
    , Border.rounded 6
    ]


lineSpacing =
    Element.spacingXY 0 10
