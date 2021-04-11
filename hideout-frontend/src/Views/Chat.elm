module Views.Chat exposing ( view )

import Chat exposing
    ( ChatId
    , ChatStatus
    , Message
    , MessageBody
    , mkJoinMsg
    , mkMessageMsg
    )
import Common.Colors exposing (..)
import Common.Styles exposing (..)
import Common.Urls exposing (..)
import CoreTypes exposing (..)
import Element
import Element exposing ( Element )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Tagged exposing ( tag, untag )
import Utils.Utils as Utils exposing (..)


view : Model -> Element Msg
view model =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacingXY 60 0
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacingXY 0 30
                , Element.scrollbarY
                ] <|
                List.map msgView <| List.reverse model.chatStatus.msgs

            , Element.el
                [ Element.width <| Element.fill
                , Element.paddingEach { top = 40, bottom = 0, left = 0, right = 0 }
                ] <|
                Input.multiline
                    [ Element.height <| Element.px 200
                    , Background.color bgColor
                    ]
                    { onChange = MessageInput
                    , text = untag model.chatStatus.input
                    , placeholder = Nothing
                    , label = Input.labelAbove [] Element.none
                    , spellcheck = False
                    }
            , Element.row
                [ Element.paddingXY 0 10 ]
                [ Input.button
                    [ Element.padding 5
                    , Background.color <| Element.rgb255 0 100 0 ]
                    { onPress = Just MessageSend
                    , label = Element.text "Send"
                    }
                ]
            ]
        , Element.column
            [ Element.width <| Element.px 400
            , Element.height Element.fill
            , Element.paddingXY 30 20
            , Border.widthEach { left = 2, right = 0, top = 0, bottom = 0 }
            ]
            [ plainPara "Users"
            ]
        ]

msgView : Message -> Element m
msgView msg =
    Element.textColumn
        [ Element.width Element.fill
        , Element.spacingXY 0 10
        ]
        [ Element.paragraph
            [ Font.bold ]
            [ Element.text  "UserX" ]
        , Element.paragraph
            []
            [ Element.text <| untag msg.body ]
        ]
