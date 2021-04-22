module Views.Chat exposing ( view, msgsViewHtmlId )

import Chat
import Common.Attributes
import Common.Colors exposing (..)
import Common.Contents exposing ( plainPara )
import Common.Styles exposing (..)
import Common.Urls exposing (..)
import CoreTypes exposing (..)
import Dict exposing ( Dict )
import Element
import Element exposing ( Element )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as JDec
import String.Extra exposing ( quote )
import Tagged exposing ( tag, untag )
import Time exposing ( millisToPosix, toSecond, utc )
import Utils.Markdown


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
                , Element.htmlAttribute <| Html.Attributes.id msgsViewHtmlId
                , Element.htmlAttribute <|
                    Html.Events.on "scroll" <| JDec.succeed <| ChatMsgsViewEvent Chat.OnManualScrolled
                ] <|
                List.map msgBundleView <| Chat.mkMsgBundles model.chatStatus.msgs

            -- New messages hint if needed
            , Element.el
                [ Element.height <| Element.px 40
                , Element.centerX
                ] <|
                case model.chatStatus.shouldHintNewMsg of
                    False -> Element.none
                    True  ->
                        Input.button
                            [ Element.centerY
                            , Element.padding 5
                            , Border.width 2
                            , Border.rounded 6
                            ]
                            { onPress = Just <| ChatMsgsViewEvent Chat.OnNewMsgHintClicked
                            , label = Element.text "New Messages Received"
                            }

            -- Input
            , Element.el
                [ Element.width <| Element.fill
                ] <|
                Input.multiline
                    [ Element.height <| Element.px 200
                    , Background.color bgColor
                    , Element.htmlAttribute <| Html.Events.onFocus <| OnChatInputFocal True
                    , Element.htmlAttribute <| Html.Events.onBlur  <| OnChatInputFocal False
                    ]
                    { onChange = MessageInput
                    , text = untag model.chatStatus.input
                    , placeholder = Nothing
                    , label = Input.labelAbove [] Element.none
                    , spellcheck = False
                    }

            -- Tools below input.
            , Element.row
                [ Element.paddingXY 0 10
                , Element.spacingXY 10 0
                ]
                [ Input.button
                    [ Element.padding 5
                    , Background.color <| Element.rgb255 0 100 0 ]
                    { onPress = Just MessageSend
                    , label = Element.text "Send"
                    }
                , Input.text
                    [ Background.color bgColor ]
                    { onChange = NewNameInput
                    , text = model.newNameInput
                    , placeholder = Nothing
                    , label = Input.labelLeft [] <|
                        Input.button
                            []
                            { onPress = Just NameChange
                            , label = Element.text "Change Name: "
                            }
                    }
                ]
            ]
        -- Right side panel.
        , Element.column
            [ Element.width <| Element.px 400
            , Element.height Element.fill
            , Element.paddingXY 30 20
            , Border.widthEach { left = 2, right = 0, top = 0, bottom = 0 }
            ] <|
            [ Element.paragraph
                [ Font.size 24 ]
                [ Element.text "Users" ]
            -- List of users
            , Element.column
                [ Element.paddingXY 0 40 
                , Element.spacingXY 0 20
                ] <|
                List.map userView <| Dict.toList model.chatStatus.users
            ]
        ]


msgBundleView : Chat.MsgBundle -> Element m
msgBundleView bundle =
    Element.textColumn
        [ Element.width Element.fill
        , Element.spacingXY 0 10
        ] <|
        [ case Chat.isMetaBundle bundle of
            False ->
                Element.paragraph
                    [ Font.bold ]
                    [ Element.text bundle.username ]

            True -> Element.none
        ] ++
        List.map msgView bundle.msgs


msgView : Chat.MsgFromServer -> Element m
msgView msg =
    let
        msgFromClient = msg.msgFromClient

        time =
            Element.el
                [ Font.size 16 ] <|
                Element.text <| String.fromInt <| toSecond utc <| millisToPosix <| msg.posixTime * 1000
    in
    case msgFromClient.msgType of
        Chat.Join ->
            Element.paragraph
                [ Font.color green ]
                [ Element.text <| msg.username ++ " joined."
                , time
                ]

        Chat.NameChange ->
            Element.paragraph
                [ Font.color yellow ]
                [ Element.text <| quote msg.username
                 ++ " changed their name to "
                 ++ ( quote <| untag msgFromClient.msgBody )
                 ++ "."
                ]

        Chat.Leave ->
            Element.paragraph
                [ Font.color red ]
                [ Element.text <| msg.username ++ " left." ]

        Chat.Content ->
            Element.column
                [] <|
                Utils.Markdown.render <| untag msgFromClient.msgBody


userView : ( Int, String ) -> Element m
userView ( userId, username ) =
    plainPara <| username ++ " (" ++ String.fromInt userId ++ ")"


msgsViewHtmlId = "chat-msgs-view"
