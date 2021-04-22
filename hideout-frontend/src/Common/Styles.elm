module Common.Styles exposing (..)

import Element
import Element.Border as Border
import Element.Font as Font


{-| Calculate window edge padding based on current window width,
and a preferred ratio that's tested on a 2560px wide monitor.
-}
windowPaddingPx windowWidth = 60 / 2560 * windowWidth


windowPadding windowWidth = Element.padding <| round <| windowPaddingPx windowWidth


widthConstraint =
    Element.width <| Element.maximum 750 Element.fill


linkStyle =
    [ Font.underline ]


buttonStyle padding =
    [ Element.padding padding
    , Border.width 2
    , Border.rounded 6
    ]


lineSpacing =
    Element.spacingXY 0 10
