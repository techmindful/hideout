port module Views.Chat exposing
    ( msgsViewHtmlId
    , handleKeyDown
    , handleKeyUp
    , update
    , view
    )

import Browser.Dom as Dom
import Browser.Navigation as Nav
import Browser.Events
import Chat exposing
    ( ElmMsg(..)
    , Model
    , MsgsViewEvent(..)
    , Status(..)
    , TypingStatus(..)
    , WsMsg(..)
    , mkTypeHintMsg
    , wsMsgDecoder
    )
import Common.Attributes
import Common.Colors exposing (..)
import Common.Contents exposing
    ( footer
    , newTabLink
    , plainPara
    )
import Common.Styles exposing (..)
import Common.Urls exposing (..)
import Dict exposing ( Dict )
import Element
import Element exposing ( Element )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Json.Decode as JDec
import List.Extra as List
import String.Extra exposing ( quote, unquote )
import Tagged exposing ( tag, untag )
import Task
import Time exposing
    ( millisToPosix
    , posixToMillis
    , toSecond
    , utc
    )
import Utils.Markdown
import Utils.Utils as Utils exposing
    ( durationSec )


port port_SendWsMsg : String -> Cmd msg
port port_NotifyChat : () -> Cmd msg


update : ElmMsg
      -> Status
      -> Browser.Events.Visibility
      -> ( Status, Cmd ElmMsg )
update elmMsg status windowVisibility =
    case status of
        Normal model ->
            updateModel elmMsg model windowVisibility

        OpeningWs chatId ->
            case elmMsg of
                OnWsReady _ ->
                    ( Normal
                        { chatId = chatId
                        , myUserId = -1

                        , input = tag ""
                        , newNameInput = ""
                        , typingStatus = NotTyping

                        , msgs = []
                        , users = Dict.empty
                        , typingUsers = []

                        , maxJoinCount = Nothing
                        , joinCount = 0

                        , hasManualScrolledUp = False
                        , shouldHintNewMsg = False

                        , isInputFocused = False
                        , isShiftHeld = False
                        }
                    , port_SendWsMsg <| Chat.mkJoinMsg chatId
                    )

                OnWsError ->
                    ( WsError, Cmd.none )

                _ ->
                    ( status, Cmd.none )

        _ ->
            ( status, Cmd.none )


updateModel : ElmMsg
           -> Model
           -> Browser.Events.Visibility
           -> ( Status, Cmd ElmMsg )
updateModel elmMsg model windowVisibility =
    case elmMsg of
        MessageInput str ->
            ( Normal
                { model |
                  input = tag str
                }
            , -- HACK: Prevent hitting enter to send the msg from triggering type hint.
              if String.endsWith "\n" str then
                  Cmd.none
              else
                  Task.perform GotInputTime Time.now
            )

        MessageSend ->
            ( -- Not clearing input field here. What if message send fails?
              Normal
                { model |
                    -- This is the right place to change typing status, instead of OnWsMsg.
                    -- Let client side change to NotTyping immediately on MessageSend.
                    -- Client won't send a "stop" type hint, if this message lags.
                    typingStatus = NotTyping
                }
            , sendChatMsg model.input
            )

        NewNameInput str ->
            ( Normal { model | newNameInput = str }
            , Cmd.none
            )

        OnNameChange ->
            if String.isEmpty model.newNameInput then
                ( Normal model, Cmd.none )
            else
                ( Normal { model | newNameInput = "" }
                , port_SendWsMsg <| Chat.mkNameChangeMsg <| tag model.newNameInput
                )

        GotInputTime inputTime ->
            ( Normal { model | typingStatus = Typing inputTime }
            , 
              -- Only send "start" type hint when user *just started* to type.
              if model.typingStatus == NotTyping then
                port_SendWsMsg <| mkTypeHintMsg True
              else
                Cmd.none
            )

        OnMsgsViewEvent event ->
            case event of
                TriedSnapScroll result ->
                    ( Normal model, Cmd.none )

                OnManualScrolled ->
                    ( Normal model
                    , Task.attempt ( OnMsgsViewEvent << GotViewport ) <|
                        Dom.getViewportOf msgsViewHtmlId
                    )

                GotViewport result ->
                    case result of
                        Err err ->
                            ( Normal model, Cmd.none )

                        Ok viewport ->
                            let
                                hasManualScrolledUp =
                                    Utils.hasManualScrolledUp viewport Chat.autoScrollMargin
                            in
                            ( Normal { model |
                                hasManualScrolledUp = hasManualScrolledUp

                                -- Some boolean logic:
                                -- Hint new msg only if it was needing the hint,
                                -- And user has manually scrolled up.
                                , shouldHintNewMsg =
                                    model.shouldHintNewMsg &&
                                    hasManualScrolledUp
                              }
                            , Cmd.none
                            )

                OnNewMsgHintClicked ->
                    ( Normal { model |
                        shouldHintNewMsg = False
                      , hasManualScrolledUp = False
                      }
                    , snapScrollChatMsgsView
                    )

        OnChatInputFocal isFocused ->
            ( Normal { model | isInputFocused = isFocused }
            , Cmd.none
            )

        -- TODO: Shouldn't receive this. How to handle?
        OnWsReady _ ->
            ( Normal model
            , Cmd.none
            )

        OnWsError -> ( WsError, Cmd.none )

        OnWsMsg str ->
            case JDec.decodeString wsMsgDecoder str of
                Err _ ->
                    ( Normal model, Debug.todo "hi" )  -- TODO: Handle error.

                Ok wsMsg ->
                    case wsMsg of
                        ChatMsgMeta_ chatMsgMeta ->
                            let
                                msgFromClient = chatMsgMeta.msgFromClient
                                senderId = chatMsgMeta.userId
                                senderName = chatMsgMeta.username
                                oldUsers = model.users

                                newUsers =
                                    case msgFromClient.msgType of
                                        Chat.Join ->
                                            Dict.insert senderId senderName oldUsers

                                        Chat.NameChange ->
                                            Dict.insert senderId
                                                ( untag msgFromClient.msgBody )
                                                oldUsers

                                        Chat.Leave ->
                                            Dict.remove senderId oldUsers

                                        _ ->
                                            model.users

                                newTypingUsers =
                                    let
                                        senderRemoved = List.remove senderId model.typingUsers
                                    in
                                    case msgFromClient.msgType of
                                        Chat.TypeHint ->
                                            if untag msgFromClient.msgBody == "start" then
                                                -- Making it unique, in case start typing cooldown and
                                                -- Stop typing cooldown mismatch, which will cause
                                                -- Duplicates in the list of typing users.
                                                model.typingUsers ++ [ senderId ] |> List.unique
                                            else
                                                senderRemoved

                                        Chat.Leave -> senderRemoved

                                        Chat.Content -> senderRemoved

                                        _ -> model.typingUsers

                                -- HACK: Because hitting Enter to send message with elm-ui
                                --       Resets type hint cooldown, we have to
                                --       Reset typing status upon receiving back our own message.
                                newTypingStatus =
                                    if msgFromClient.msgType == Chat.Content && isMyMsg then
                                        NotTyping
                                    else
                                        model.typingStatus

                                newMsgs =
                                    case msgFromClient.msgType of
                                        Chat.TypeHint ->
                                            model.msgs

                                        _ ->
                                            model.msgs ++ [ chatMsgMeta ]

                                isMyMsg : Bool
                                isMyMsg = chatMsgMeta.userId == model.myUserId
                            in
                            ( Normal
                                { model |
                                  msgs = newMsgs
                                , users = newUsers
                                , typingUsers = newTypingUsers
                                , typingStatus = newTypingStatus
                                , shouldHintNewMsg =
                                    -- If hint is previously needed, keep it.
                                    model.shouldHintNewMsg || 
                                    ( model.hasManualScrolledUp &&
                                      not isMyMsg &&
                                      msgFromClient.msgType /= Chat.TypeHint
                                    )

                                -- User ID can tell join count.
                                , joinCount =
                                    if msgFromClient.msgType == Chat.Join then
                                        senderId + 1
                                    else
                                        model.joinCount

                                -- Clear input field if it's a content msg we sent
                                -- And it's confirmed that server already received it.
                                , input = if isMyMsg &&
                                             msgFromClient.msgType == Chat.Content
                                          then tag ""
                                          else model.input
                                }
                            , Cmd.batch
                                [ if not model.hasManualScrolledUp then
                                    snapScrollChatMsgsView
                                  else
                                    Cmd.none

                                , if windowVisibility == Browser.Events.Hidden &&
                                     msgFromClient.msgType /= Chat.TypeHint then
                                    port_NotifyChat ()
                                  else
                                    Cmd.none
                                ]
                            )

                        Chat.CtrlMsg_ ctrlMsg ->
                            case ctrlMsg of
                                Chat.Err_ err ->
                                    ( ChatError err
                                    , Cmd.none
                                    )

                        Chat.MsgHistory_ msgHistory ->
                            ( Normal
                                { model |
                                  msgs = msgHistory.msgs
                                , users = msgHistory.users
                                , maxJoinCount = msgHistory.maxJoinCount
                                }
                           , Cmd.none
                           )

                        Chat.UserIdMsg_ userIdMsg ->
                            ( Normal { model | myUserId = userIdMsg.yourUserId }
                            , Cmd.none
                            )

        GotTime time ->
            let
                -- Change typing status if user was typing,
                -- But haven't typed in 5 seconds.
                newTypingStatus =
                    case model.typingStatus of
                        NotTyping -> NotTyping
                        Typing lastInputTime ->
                            if durationSec lastInputTime time >= 5 then
                                NotTyping
                            else
                                model.typingStatus
            in
            ( Normal { model | typingStatus = newTypingStatus }
            , 
              -- Send "stop" type hint if user *just stopped* typing.
              if model.typingStatus /= NotTyping && newTypingStatus == NotTyping then
                port_SendWsMsg <| mkTypeHintMsg False
              else
                Cmd.none
            )


{-| This is part of update. -}
handleKeyDown : Status -> String -> ( Status, Cmd ElmMsg )
handleKeyDown status key =
    case status of
        Normal model ->
            case key of
                "Enter" ->
                    ( Normal model
                    , if ( not model.isShiftHeld ) && model.isInputFocused then
                        sendChatMsg model.input
                      else
                        Cmd.none
                    )

                "Shift" ->
                    ( Normal { model | isShiftHeld = True }, Cmd.none )

                _ -> ( status, Cmd.none )

        _ -> ( status, Cmd.none )


handleKeyUp : Status -> String -> ( Status, Cmd ElmMsg )
handleKeyUp status key =
    case status of
        Normal model ->
            case key of
                "Shift" ->
                    ( Normal { model | isShiftHeld = False }, Cmd.none )

                _ -> ( status, Cmd.none )

        _ -> ( status, Cmd.none )


view : Status -> Float -> Element ElmMsg
view status viewportWidth =
    case status of
        Normal model ->
            chatView model viewportWidth

        OpeningWs chatId ->
            mkErrView <|
                plainPara "Waiting for websocket to be opened..."

        WsError ->
            mkErrView <|
                plainPara
                    """
                    Error when opening websocket. Maybe server is unreachable?
                    """

        ChatError err ->
            let
                errContent = case err of
                    Chat.MaxJoined ->
                        Element.paragraph
                            []
                            [ Element.text
                                """
                                Hi, welcome to Hideout! Unfortunately, this chat room has reached the maximum number of times it can be joined. Read more about what this implies 
                                """
                            , newTabLink aboutUrl "here"
                            , Element.text "."
                            ]

                    Chat.NotFound ->
                        Element.column
                            []
                            [ plainPara
                                """
                                Hi, welcome to Hideout! This chat room doesn't exist. The reason can be:
                                """
                            , Element.column
                                [ Element.paddingXY 0 20
                                , Element.spacingXY 0 10
                                ]
                                [ plainPara "- Either you entered a wrong link or chat ID;"
                                , plainPara "- Or the chat is expired and deleted."
                                ]
                            ]
            in
            mkErrView errContent

        NotChatting ->
            mkErrView <| plainPara "Chat status is NotChatting. Internal logical error?"


chatView : Model -> Float -> Element ElmMsg
chatView model viewportWidth =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacingXY sideColumnGap 0
        ]
        [ Element.html <|
            Html.audio
                [ Html.Attributes.id "notificationAudio" ]
                [ Html.source
                    [ Html.Attributes.src
                        "/static/sounds/notification.wav"
                    , Html.Attributes.type_ "audio/wav"
                    ]
                    []
                ]

        , Element.column
            [ Element.width <| Element.maximum
                ( chatColumnMaxWidthPx viewportWidth )
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
                    Html.Events.on "scroll" <| JDec.succeed <| OnMsgsViewEvent OnManualScrolled
                ] <|
                List.map msgBundleView <| Chat.mkMsgBundles model.msgs

            -- Type hint
            , Element.el
                [ Element.height <| Element.px 20
                , Element.paddingXY 0 10
                , Font.color grey
                ]
                ( if List.length model.typingUsers == 0 then
                    Element.none
                  else
                    let
                        typingUsersNames = 
                            List.map
                                ( \userId ->
                                    Dict.get userId model.users |>
                                    Maybe.withDefault "[ErrorUsername]"
                                )
                                model.typingUsers
                    in
                    Element.text <|
                        ( String.join ", " typingUsersNames ) ++
                        ( if List.length model.typingUsers == 1 then
                            " is "
                          else
                            " are "
                        ) ++
                        "typing..."
                )


            -- New messages hint if needed
            , Element.el
                [ Element.height <| Element.px 40
                , Element.centerX
                ] <|
                case model.shouldHintNewMsg of
                    False -> Element.none
                    True  ->
                        Input.button
                            [ Element.centerY
                            , Element.padding 5
                            , Border.width 2
                            , Border.rounded 6
                            ]
                            { onPress = Just <| OnMsgsViewEvent OnNewMsgHintClicked
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
                    , text = untag model.input
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
                            { onPress = Just OnNameChange
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
                List.map userView <| Dict.toList model.users
            -- Room info
            , Element.column
                [ Element.height <| Element.fillPortion 1
                , Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
                , Element.spacingXY 0 10
                , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
                ]
                [ plainPara <| "Current Users: "
                            ++ ( String.fromInt <| Dict.size model.users )
                , plainPara <| "Join Count: " ++ String.fromInt model.joinCount
                , plainPara <|
                    "Max Join Count: " ++ case model.maxJoinCount of
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

        Chat.TypeHint ->
            Element.none


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


sendChatMsg : Chat.MsgBody -> Cmd ElmMsg
sendChatMsg msgBody =
    if String.isEmpty <| untag msgBody then
        Cmd.none
    else
        port_SendWsMsg <| Chat.mkContentMsg msgBody


snapScrollChatMsgsView : Cmd ElmMsg
snapScrollChatMsgsView =
    Dom.getViewportOf msgsViewHtmlId
        |> Task.andThen
            ( \ viewport ->
                Dom.setViewportOf msgsViewHtmlId 0 viewport.scene.height
            )
        |> ( Task.attempt <| Chat.OnMsgsViewEvent << Chat.TriedSnapScroll )


mkErrView : Element msg -> Element msg
mkErrView content =
    Element.column
        [ Element.width <| Element.maximum 750 Element.fill
        , Element.spacingXY 0 100
        ]
        [ content
        , footer
        ]

