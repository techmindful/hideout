module Common.Styles exposing (..)

import Common.Colors exposing (..)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


{-| Calculate window edge padding based on current window width,
and a preferred ratio that's tested on a 2560px wide monitor.
-}
windowPaddingPx windowWidth = 60 / 2560 * windowWidth


windowPadding windowWidth = Element.padding <| round <| windowPaddingPx windowWidth


widthConstraint =
    Element.width <| Element.maximum 750 Element.fill


linkStyle = [ Font.underline ]


roundedBorder padding =
    [ Element.padding padding
    , Border.width 2
    , Border.rounded 6
    ]


buttonStyle = roundedBorder


lineSpacing =
    Element.spacingXY 0 10


paraSpacing =
    Element.spacingXY 0 20


inlineInputStyle : List ( Element.Attribute msg )
inlineInputStyle =
    [ Element.width <| Element.px 100
    , Element.height <| Element.maximum 40 Element.fill
    , Background.color bgColor
    ]
