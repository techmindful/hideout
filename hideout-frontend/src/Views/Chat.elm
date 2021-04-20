module Views.Chat exposing ( view )

import Chat
import Common.Colors exposing (..)
import Common.Contents exposing ( plainPara )
import Common.Styles exposing (..)
import Common.Urls exposing (..)
import CoreTypes exposing (..)
import Element
import Element exposing ( Element )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import String.Extra exposing ( quote )
import Tagged exposing ( tag, untag )
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
                ] <|
                List.map msgBundleView <| Chat.mkMsgBundles model.chatStatus.msgs

            -- Input
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
                List.map userView model.chatStatus.users
            ]
        ]


msgBundleView : Chat.MsgBundle -> Element m
msgBundleView bundle =
    Element.textColumn
        [ Element.width Element.fill
        , Element.spacingXY 0 10
        ] <|
        [ Element.paragraph
            [ Font.bold ]
            [ Element.text bundle.username ]
        ] ++
        List.map msgView bundle.msgs


msgView : Chat.MsgFromServer -> Element m
msgView msg =
    let msgFromClient = msg.msgFromClient
    in
    case untag msgFromClient.msgType of
        "join" ->
            Element.paragraph
                [ Font.color green ]
                [ Element.text <| msg.username ++ " joined." ]

        "nameChange" ->
            Element.paragraph
                [ Font.color yellow ]
                [ Element.text <|
                    ( quote msg.username )
                 ++ " changed their name to "
                 ++ ( quote <| untag msgFromClient.msgBody )
                 ++ "."
                ]

        "leave" ->
            Element.paragraph
                [ Font.color red ]
                [ Element.text <| msg.username ++ " left." ]

        _ ->
            Element.column
                [] <|
                Utils.Markdown.render <| untag msgFromClient.msgBody


userView : String -> Element m
userView username =
    plainPara username
