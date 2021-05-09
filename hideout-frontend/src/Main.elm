port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Chat
import Common.Colors exposing (..)
import Common.Contents exposing
    ( plainPara
    , posIntInputHint
    )
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
import Html
import Http
import Json.Encode as JEnc
import Json.Decode as JDec
import Letter exposing (..)
import List.Extra as List
import Route exposing (..)
import String.Extra exposing (unquote)
import Tagged exposing ( tag, untag )
import Task
import Url exposing (Url)
import Url.Parser
import UserStatus exposing (..)
import Utils.Markdown
import Utils.Types exposing ( PosIntInput(..), posIntInputToStr, strToPosIntInput )
import Utils.Utils as Utils
import Views.About
import Views.Chat
import Views.ConfigChat
import Views.ReadLetter
import Views.WriteLetter


port port_InitWs : String -> Cmd msg
port port_WsReady : ( String -> msg ) -> Sub msg
port port_SendWsMsg : String -> Cmd msg
port port_RecvWsMsg : ( String -> msg ) -> Sub msg
port port_DebugLog : String -> Cmd msg


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.batch
        [ port_WsReady OnWsReady
        , port_RecvWsMsg OnWsMsg

        , Browser.Events.onResize ( \_ _ -> OnWindowResized )

        , Browser.Events.onKeyDown <|
            JDec.map OnKeyDown <| JDec.field "key" JDec.string

        , Browser.Events.onKeyUp <|
            JDec.map OnKeyUp <| JDec.field "key" JDec.string
        ]

init : JDec.Value -> Url -> Nav.Key -> ( State, Cmd Msg )
init jsonFlag url navKey =
    let
        tryInitModel : Result JDec.Error ( Model, Cmd Msg )
        tryInitModel =
            Result.map
                ( \initFlag -> initModel initFlag url navKey )
                ( JDec.decodeValue initFlagDecoder jsonFlag )
    in
    case tryInitModel of
        Ok ( model, cmd ) ->
            ( Normal model, cmd )

        Err decErr ->
            ( ErrGetHost, Cmd.none )


initModel : InitFlag -> Url -> Nav.Key -> ( Model, Cmd Msg )
initModel initFlag url navKey =
    let
        route = getRoute url

        userStatus = routeToInitUserStatus route
    in
    ( { protocol = initFlag.protocol
      , host = initFlag.host
      , origin = initFlag.protocol ++ "//" ++ initFlag.host
      , route = route

      , viewport =
           { scene = { width = 1920, height = 1080 }
           , viewport = { x = 0, y = 0, width = 1920, height = 1080 }
           }
      , navKey = navKey
      , isWsReady = False
      , userStatus = userStatus

      , joinChatInput = ""

      , letterRawInput = { body = "", maxReadCount = "1" }
      , letterPersistInput = True
      , letterStatus = { read = Init, write = Letter.NotSent }

      , dispChatMaxJoinCountInput = "2"
      , persistChatMaxJoinCountInput = "2"
      , chatStatus =
          { chatId = tag ""
          , myUserId = -1
          , msgs = []
          , input = tag ""
          , users = Dict.empty

          , maxJoinCount = Nothing
          , joinCount = 0

          , hasManualScrolledUp = False
          , shouldHintNewMsg = False

          , isInputFocused = False

          , err = Nothing
          }
      , newNameInput = ""

      , isShiftHeld = False

      , tempResp = ""
      }
    , Cmd.batch
        [ getViewportCmd

        , case route of
            Chat chatIdStr ->
                port_InitWs chatIdStr

            ReadLetter letterId ->
                getLetterReq letterId

            _ -> Cmd.none
        ]
    )


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case state of
        Normal model ->
            updateModel msg model

        _ ->
            ( state, Cmd.none )


updateModel : Msg -> Model -> ( State, Cmd Msg )
updateModel msg ( { letterRawInput, letterStatus, chatStatus } as model ) =
    case msg of
        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( Normal model, Nav.pushUrl model.navKey <| Url.toString url )

                Browser.External urlStr ->
                    ( Normal model, Nav.load urlStr )

        UrlChanged url ->
            let route = getRoute url
                userStatus = routeToInitUserStatus route
            in
            ( Normal { model| route = route
                      , userStatus = userStatus              
              }
            , case route of
                Chat chatIdStr ->
                    port_InitWs chatIdStr

                ReadLetter letterId ->
                    getLetterReq letterId

                _ -> Cmd.none
            )

        GotViewport viewport ->
            ( Normal { model| viewport = viewport }, Cmd.none )

        GotReadLetterResp result ->
            ( Normal { model| letterStatus =
                { letterStatus | read = Got result }
              }
            , Cmd.none
            )

        JoinChatInput str ->
            ( Normal { model | joinChatInput = str }, Cmd.none )

        JoinChat ->
            ( Normal model
            , Nav.pushUrl model.navKey <| frontendChatUrl model.joinChatInput
            )

        LetterInput str ->
            ( Normal { model| letterRawInput =
                { letterRawInput | body = str }
              }
            , Cmd.none
            )

        LetterMaxReadCountInput str ->
            ( Normal { model| letterRawInput =
                { letterRawInput | maxReadCount = str }
              }
            , Cmd.none
            )

        LetterPersistInput input ->
            ( Normal { model| letterPersistInput = input }, Cmd.none )

        LetterSend ->
            case Letter.validateInput letterRawInput of
                Err _ -> ( Normal model, Cmd.none )
                Ok goodInput ->
                    ( Normal { model|
                        letterStatus =
                          { letterStatus | write =
                              Letter.Sent { maxReadCount = goodInput.maxReadCount }
                          }
                      , letterRawInput =
                          { letterRawInput | body = "", maxReadCount = "1" }
                      }
                    , Http.request
                        { method = "PUT"
                        , headers = []
                        , url = backendWriteLetterUrl
                        , body =
                            Http.jsonBody <|
                                JEnc.object
                                    [ ( "body", JEnc.string goodInput.body )
                                    , ( "maxReadCount", JEnc.int goodInput.maxReadCount )
                                    , ( "persist", JEnc.bool model.letterPersistInput )
                                    ]
                        , expect = Http.expectString GotLetterSendResp
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

        GotLetterSendResp result ->
            case model.letterStatus.write of
                Letter.Sent info ->
                    let
                        newModel = case result of
                            Err err ->
                                Normal { model| letterStatus =
                                    { letterStatus | write = GotResp <| Err err }
                                }

                            Ok letterId ->
                                Normal { model| letterStatus =
                                    { letterStatus | write =
                                        GotResp <| Ok
                                            { id = letterId
                                            , maxReadCount = info.maxReadCount
                                            }
                                    }
                                }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( Normal model
                    , Debug.todo
                        "Incorrect LetterStatus when letter send is responeded."
                    )

        DispChatMaxJoinCountInput str ->
            ( Normal { model| dispChatMaxJoinCountInput = str }
            , Cmd.none
            )

        SpawnDispChat ->
            case strToPosIntInput model.dispChatMaxJoinCountInput of
                Bad  _ -> ( Normal model, Cmd.none )
                Good posInt ->
                    ( Normal model
                    , Http.request
                        { method = "PUT"
                        , headers = []
                        , url = backendSpawnDispChatUrl 
                        , body = Http.stringBody "text/plain;charset=utf-8" <| String.fromInt posInt
                        , expect = Http.expectString GotSpawnDispChatResp 
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

        GotSpawnDispChatResp result ->
            case result of
                Err _ ->
                    ( Normal model, Cmd.none )

                Ok chatId ->
                    ( Normal { model| chatStatus =
                        { chatStatus | chatId = tag <| unquote chatId }
                      }
                    , Nav.pushUrl model.navKey <| frontendChatUrl <| unquote chatId
                    )

        PersistChatMaxJoinCountInput str ->
            ( Normal { model| persistChatMaxJoinCountInput = str }
            , Cmd.none
            )

        SpawnPersistChat ->
            ( Normal model
            , case strToPosIntInput model.persistChatMaxJoinCountInput of
                Bad _ -> Cmd.none
                Good posInt ->
                    Http.request
                        { method = "PUT"
                        , headers = []
                        , url = backendSpawnPersistChatUrl
                        , body = Http.stringBody "text/plain;charset=utf-8" <| String.fromInt posInt
                        , expect = Http.expectString GotSpawnPersistChatResp 
                        , timeout = Nothing
                        , tracker = Nothing
                        }
            )


        GotSpawnPersistChatResp result ->
            ( Normal { model| userStatus = GotPersistChatIdLetter result }
            , Cmd.none
            )


        MessageInput str ->
            ( Normal { model| chatStatus = { chatStatus | input = tag str } }
            , Cmd.none
            )

        MessageSend ->
            ( Normal model  -- Not clearing input field here. What if message send fails?
            , sendChatMsg model.chatStatus.input
            )

        NewNameInput str ->
            ( Normal { model| newNameInput = str }
            , Cmd.none
            )

        NameChange ->
            if String.isEmpty model.newNameInput then
                ( Normal model, Cmd.none )
            else
                ( Normal { model| newNameInput = "" }
                , port_SendWsMsg <| Chat.mkNameChangeMsg <| tag model.newNameInput
                )

        OnWsReady _ ->
            ( Normal { model| isWsReady = True }
            -- If ws is open after user lands on the chat page,
            -- Send the join msg.
            , case model.route of
                Chat chatIdStr -> port_SendWsMsg <| Chat.mkJoinMsg <| tag chatIdStr
                _ -> Cmd.none
            )

        OnWsMsg str ->
            case JDec.decodeString Chat.wsMsgDecoder str of
                Err _ ->
                    ( Normal model, Debug.todo "hi" )  -- TODO: Handle error.

                Ok wsMsg ->
                    case wsMsg of
                        Chat.ChatMsgMeta_ chatMsgMeta ->
                            let
                                msgFromClient = chatMsgMeta.msgFromClient
                                senderId = chatMsgMeta.userId
                                senderName = chatMsgMeta.username
                                oldUsers = model.chatStatus.users

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

                                        Chat.Content ->
                                            model.chatStatus.users

                                isMyMsg : Bool
                                isMyMsg = chatMsgMeta.userId == model.chatStatus.myUserId
                            in
                            ( Normal { model|
                                chatStatus =
                                    { chatStatus |
                                      msgs = chatStatus.msgs ++ [ chatMsgMeta ]
                                    , users = newUsers
                                    , shouldHintNewMsg =
                                        -- If hint is previously needed, keep it.
                                        model.chatStatus.shouldHintNewMsg || 
                                        ( model.chatStatus.hasManualScrolledUp &&
                                          ( not isMyMsg )
                                        )

                                    -- User ID can tell join count.
                                    , joinCount =
                                        if msgFromClient.msgType == Chat.Join then
                                            senderId + 1
                                        else
                                            model.chatStatus.joinCount

                                    -- Clear input field if it's a content msg we sent
                                    -- And it's confirmed that server already received it.
                                    , input = if isMyMsg &&
                                                 msgFromClient.msgType == Chat.Content
                                              then tag ""
                                              else model.chatStatus.input
                                    }
                              }
                            , if not model.chatStatus.hasManualScrolledUp then
                                snapScrollChatMsgsView
                              else
                                Cmd.none
                            )

                        Chat.CtrlMsg_ ctrlMsg ->
                            case ctrlMsg of
                                Chat.Err_ err ->
                                    ( Normal { model| chatStatus =
                                        { chatStatus | err = Just err }
                                      }
                                    , Cmd.none
                                    )

                        Chat.MsgHistory_ msgHistory ->
                           ( Normal { model|
                              chatStatus =
                                  { chatStatus | msgs  = msgHistory.msgs
                                               , users = msgHistory.users
                                               , maxJoinCount = msgHistory.maxJoinCount
                                  }
                             }
                           , Cmd.none
                           )

                        Chat.UserIdMsg_ userIdMsg ->
                            ( Normal { model| chatStatus =
                                { chatStatus | myUserId = userIdMsg.yourUserId }
                              }
                            , Cmd.none
                            )

        ChatMsgsViewEvent event ->
            case event of
                Chat.TriedSnapScroll result ->
                    ( Normal model, Cmd.none )

                Chat.OnManualScrolled ->
                    ( Normal model
                    , Task.attempt ( ChatMsgsViewEvent << Chat.GotViewport ) <|
                        Dom.getViewportOf Views.Chat.msgsViewHtmlId
                    )

                Chat.GotViewport result ->
                    case result of
                        Err err ->
                            ( Normal model, port_DebugLog <| Debug.toString err )

                        Ok viewport ->
                            let
                                logViewport =
                                    port_DebugLog <| String.fromFloat viewport.viewport.y ++ ", "
                                                  ++ String.fromFloat viewport.viewport.height ++ ", "
                                                  ++ String.fromFloat viewport.scene.height

                                hasManualScrolledUp =
                                    Utils.hasManualScrolledUp viewport Chat.autoScrollMargin
                            in
                            ( Normal { model| chatStatus =
                                { chatStatus |
                                  hasManualScrolledUp = hasManualScrolledUp

                                -- Some boolean logic:
                                -- Hint new msg only if it was needing the hint,
                                -- And user has manually scrolled up.
                                , shouldHintNewMsg =
                                    model.chatStatus.shouldHintNewMsg &&
                                    hasManualScrolledUp
                                }
                              }
                            , Cmd.none
                            )

                Chat.OnNewMsgHintClicked ->
                    ( Normal { model| chatStatus =
                        { chatStatus | shouldHintNewMsg = False
                                     , hasManualScrolledUp = False
                        }
                      }
                    , snapScrollChatMsgsView
                    )

        OnChatInputFocal isFocused ->
            ( Normal { model| chatStatus =
                { chatStatus | isInputFocused = isFocused }
              }
            , Cmd.none
            )

        OnWindowResized ->
            ( Normal model, getViewportCmd )

        OnKeyDown key ->
            case key of
                "Enter" ->
                    ( Normal model
                    , if ( not model.isShiftHeld ) && model.chatStatus.isInputFocused then
                        sendChatMsg model.chatStatus.input
                      else
                        Cmd.none
                    )

                "Shift" ->
                    ( Normal { model| isShiftHeld = True }, Cmd.none )

                _ ->
                    ( Normal model, Cmd.none )

        OnKeyUp key ->
            case key of
                "Shift" ->
                    ( Normal { model| isShiftHeld = False }, Cmd.none )

                _ ->
                    ( Normal model, Cmd.none )

        Nop ->
            ( Normal model, Cmd.none )


view : State -> Browser.Document Msg
view state =
    case state of
        Normal model ->
            viewModel model

        _ ->
            { title = "Hideout got an error D:"
            , body =
                [ Element.layout
                    [ Background.color bgColor
                    , Font.color white
                    ] <|
                    Element.text "Error state!"
                ]
            }


viewModel : Model -> Browser.Document Msg
viewModel model =
    { title = "Hideout"
    , body =
        [ Element.layout
            [ Background.color bgColor
            , Font.color white
            ]
            ( Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , windowPadding model.viewport.viewport.width
                ]
                ( case model.route of
                    Root ->
                        Element.textColumn
                            []
                            [ plainPara "Welcome to Hideout - A Service for Disposable Messages!"
                            , Element.textColumn
                                [ Element.paddingXY 0 40
                                , Element.spacingXY 0 10
                                ]
                                [ Element.link
                                    []
                                    { url = frontendWriteLetterUrl
                                    , label = Element.text "> Write a letter."
                                    }
                                , Element.link
                                    []
                                    { url = configChatUrl 
                                    , label = Element.text "> Start a chat."
                                    }
                                , Element.row
                                    [ Element.paddingXY 0 10 ]
                                    [ Element.text "> Join a chat: "
                                    , Input.text
                                        [ Element.width <| Element.px 400
                                        , Element.height <| Element.maximum 40 Element.fill
                                        , Background.color bgColor
                                        ]
                                        { onChange = JoinChatInput
                                        , text = model.joinChatInput
                                        , placeholder = Just <|
                                            Input.placeholder
                                                [] <|
                                                Element.text "Room ID"
                                        , label = Input.labelHidden ""
                                        }
                                    , Element.el
                                        [ Element.paddingXY 10 0
                                        ] <|
                                        Input.button
                                            [ Element.padding 5
                                            , Border.width 2
                                            , Border.rounded 6
                                            ]
                                            { onPress = Just JoinChat
                                            , label = Element.text "Join"
                                            }
                                    ]
                                , Element.link
                                    []
                                    { url = aboutUrl
                                    , label = Element.text "> How does it work?"
                                    }
                                ]
                            ]

                    About -> Views.About.view model.viewport.viewport.width

                    ReadLetter id -> Views.ReadLetter.view model
                            
                    WriteLetter -> Views.WriteLetter.view model

                    Chat chatId -> Views.Chat.view model

                    ConfigChat -> Views.ConfigChat.view model

                    NotFound ->
                        Element.text "404"
                )
            )
        ]
    }


routeToInitUserStatus : Route -> UserStatus
routeToInitUserStatus route =
    case route of
        _ -> Other


getLetterReq : String -> Cmd Msg
getLetterReq letterId =
    Http.get
        { url = backendReadLetterUrl ++ "/" ++ letterId
        , expect = Http.expectJson GotReadLetterResp letterMetaJsonDec
        }


getViewportCmd : Cmd Msg
getViewportCmd = Task.perform GotViewport Dom.getViewport


snapScrollChatMsgsView : Cmd Msg
snapScrollChatMsgsView =
    Dom.getViewportOf Views.Chat.msgsViewHtmlId
        |> Task.andThen
            ( \ viewport ->
                Dom.setViewportOf
                    Views.Chat.msgsViewHtmlId 0 viewport.scene.height
            )
        |> ( Task.attempt <| ChatMsgsViewEvent << Chat.TriedSnapScroll )


sendChatMsg : Chat.MsgBody -> Cmd Msg
sendChatMsg msgBody =
    if String.isEmpty <| untag msgBody then
        Cmd.none
    else
        port_SendWsMsg <| Chat.mkContentMsg msgBody


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
