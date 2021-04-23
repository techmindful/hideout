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
import Utils.Utils as Utils


view : Model -> Element Msg
view model =
    case model.chatStatus.err of
        Nothing  -> chatView model
        Just err ->
            Element.el
                [ Element.width <| Element.maximum 750 Element.fill ] <|
                case err of
                    Chat.MaxJoined ->
                        plainPara
                        """
                        Hi, welcome to Hideout! Unfortunately, this chat room has reached the maximum number of times it can be joined. This means some other participants have joined more than once, by accidentally reloading the page etc. The initiator of the chat needs to make a new chat room, and make sure nobody joins more than once.
                        """


chatView : Model -> Element Msg
chatView model =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacingXY sideColumnGap 0
        ]
        [ Element.column
            [ Element.width <| Element.maximum
                ( chatColumnMaxWidthPx model.viewport.viewport.width )
                Element.fill
            , Element.height Element.fill
            ]
            [ Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.paddingEach { right = 30, left = 0, top = 0, bottom = 0 }
                , Element.spacingXY 0 30  -- Spacing between msg bundles.
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
            [ Element.width <| Element.px sideColumnWidthPx
            , Element.height Element.fill
            , Element.paddingXY 30 20
            , Border.widthEach { left = 2, right = 0, top = 0, bottom = 0 }
            ]
            [ Element.paragraph
                [ Font.size 24 ]
                [ Element.text "Users" ]
            -- List of users
            , Element.column
                [ Element.width Element.fill
                , Element.height <| Element.fillPortion 4
                , Element.paddingXY 0 40 
                , Element.spacingXY 0 20
                , Element.scrollbarY
                ] <|
                List.map userView <| Dict.toList model.chatStatus.users
            -- Room info
            , Element.column
                [ Element.height <| Element.fillPortion 1
                , Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
                , Element.spacingXY 0 10
                , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
                ]
                [ plainPara <| "Current Users: "
                            ++ ( String.fromInt <| Dict.size model.chatStatus.users )
                , plainPara <| "Join Count: " ++ String.fromInt model.chatStatus.joinCount
                , plainPara <|
                    "Max Join Count: " ++ case model.chatStatus.maxJoinCount of
                        Nothing -> "Unlimited"
                        Just n  -> String.fromInt n
                ]
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
                Element.row
                    [ Element.spacingXY 10 0 ]
                    [ Element.paragraph
                        [ Element.width Element.shrink
                        , Font.bold
                        ]
                        [ Element.text bundle.username ]
                    , Common.Contents.timeText bundle.time bundle.time
                    ]

            True -> Element.none
        ] ++
        List.map msgView bundle.msgs


msgView : Chat.ChatMsgMeta -> Element m
msgView msg =
    let
        msgFromClient = msg.msgFromClient

        paddedTimeText =
            Element.el
                [ Element.paddingEach { left = 10, right = 0, top = 0, bottom = 0 } ] <|
                Common.Contents.timeText ( Utils.posixSecToPosix msg.posixTimeSec ) ( Utils.posixSecToPosix msg.posixTimeSec )
    in
    case msgFromClient.msgType of
        Chat.Join ->
            Element.paragraph
                [ Font.color green ]
                [ Element.text <| msg.username ++ " joined."
                , paddedTimeText
                ]

        Chat.NameChange ->
            Element.paragraph
                [ Font.color yellow ]
                [ Element.text <| quote msg.username
                 ++ " changed their name to "
                 ++ ( quote <| untag msgFromClient.msgBody )
                 ++ "."
                , paddedTimeText
                ]

        Chat.Leave ->
            Element.paragraph
                [ Font.color red ]
                [ Element.text <| msg.username ++ " left."
                , paddedTimeText
                ]

        Chat.Content ->
            Element.column
                [] <|
                ( Utils.Markdown.render <| untag msgFromClient.msgBody )


userView : ( Int, String ) -> Element m
userView ( userId, username ) =
    plainPara <| username ++ " (" ++ String.fromInt userId ++ ")"


sideColumnWidthPx : Int
sideColumnWidthPx = 400


sideColumnGap : Int
sideColumnGap = 60


chatColumnMaxWidthPx : Float -> Int
chatColumnMaxWidthPx windowWidth =
    round <|
        windowWidth - ( Common.Styles.windowPaddingPx windowWidth ) * 2
                    - toFloat sideColumnWidthPx
                    - toFloat sideColumnGap


msgsViewHtmlId = "chat-msgs-view"
