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
import Views.Chat
import Views.ConfigChat
import Views.ReadLetter
import Views.WriteLetter


port port_InitWs : String -> Cmd msg
port port_WsReady : ( String -> msg ) -> Sub msg
port port_SendWsMsg : String -> Cmd msg
port port_RecvWsMsg : ( String -> msg ) -> Sub msg
port port_DebugLog : String -> Cmd msg


subscriptions : Model -> Sub Msg
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

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =

    let route = getRoute url
        userStatus = routeToInitUserStatus route
    in
    ( { route = route
      , viewport =
           { scene = { width = 1920, height = 1080 }
           , viewport = { x = 0, y = 0, width = 1920, height = 1080 }
           }
      , navKey = navKey
      , isWsReady = False
      , userStatus = userStatus

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( { letterRawInput, letterStatus, chatStatus } as model ) =
    case msg of
        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                Browser.External urlStr ->
                    ( model, Nav.load urlStr )

        UrlChanged url ->
            let route = getRoute url
                userStatus = routeToInitUserStatus route
            in
            ( { model | route = route
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
            ( { model | viewport = viewport }, Cmd.none )

        GotReadLetterResp result ->
            ( { model | letterStatus =
                { letterStatus | read = Got result }
              }
            , Cmd.none
            )

        LetterInput str ->
            ( { model | letterRawInput =
                { letterRawInput | body = str }
              }
            , Cmd.none
            )

        LetterMaxReadCountInput str ->
            ( { model | letterRawInput =
                { letterRawInput | maxReadCount = str }
              }
            , Cmd.none
            )

        LetterPersistInput input ->
            ( { model | letterPersistInput = input }, Cmd.none )

        LetterSend ->
            case Letter.validateInput letterRawInput of
                Err _ -> ( model, Cmd.none )
                Ok goodInput ->
                    ( { model |
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
                                { model | letterStatus =
                                    { letterStatus | write = GotResp <| Err err }
                                }

                            Ok letterId ->
                                { model | letterStatus =
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
                    ( model
                    , Debug.todo
                        "Incorrect LetterStatus when letter send is responeded."
                    )

        DispChatMaxJoinCountInput str ->
            ( { model | dispChatMaxJoinCountInput = str }
            , Cmd.none
            )

        SpawnDispChat ->
            case strToPosIntInput model.dispChatMaxJoinCountInput of
                Bad  _ -> ( model, Cmd.none )
                Good posInt ->
                    ( model
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
                    ( model, Cmd.none )

                Ok chatId ->
                    ( { model | chatStatus =
                        { chatStatus | chatId = tag <| unquote chatId }
                      }
                    , Nav.pushUrl model.navKey <| frontendChatUrl <| unquote chatId
                    )

        PersistChatMaxJoinCountInput str ->
            ( { model | persistChatMaxJoinCountInput = str }
            , Cmd.none
            )

        SpawnPersistChat ->
            ( model
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
            ( { model | userStatus = GotPersistChatIdLetter result }
            , Cmd.none
            )


        MessageInput str ->
            ( { model | chatStatus = { chatStatus | input = tag str } }
            , Cmd.none
            )

        MessageSend ->
            ( model  -- Not clearing input field here. What if message send fails?
            , sendChatMsg model.chatStatus.input
            )

        NewNameInput str ->
            ( { model | newNameInput = str }
            , Cmd.none
            )

        NameChange ->
            if String.isEmpty model.newNameInput then
                ( model, Cmd.none )
            else
                ( { model | newNameInput = "" }
                , port_SendWsMsg <| Chat.mkNameChangeMsg <| tag model.newNameInput
                )

        OnWsReady _ ->
            ( { model | isWsReady = True }
            -- If ws is open after user lands on the chat page,
            -- Send the join msg.
            , case model.route of
                Chat chatIdStr -> port_SendWsMsg <| Chat.mkJoinMsg <| tag chatIdStr
                _ -> Cmd.none
            )

        OnWsMsg str ->
            case JDec.decodeString Chat.wsMsgDecoder str of
                Err _ ->
                    ( model, Debug.todo "hi" )  -- TODO: Handle error.

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
                            ( { model |
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
                                    ( { model | chatStatus =
                                        { chatStatus | err = Just err }
                                      }
                                    , Cmd.none
                                    )

                        Chat.MsgHistory_ msgHistory ->
                           ( { model |
                              chatStatus =
                                  { chatStatus | msgs  = msgHistory.msgs
                                               , users = msgHistory.users
                                               , maxJoinCount = msgHistory.maxJoinCount
                                  }
                             }
                           , Cmd.none
                           )

                        Chat.UserIdMsg_ userIdMsg ->
                            ( { model | chatStatus =
                                { chatStatus | myUserId = userIdMsg.yourUserId }
                              }
                            , Cmd.none
                            )

        ChatMsgsViewEvent event ->
            case event of
                Chat.TriedSnapScroll result ->
                    ( model, Cmd.none )

                Chat.OnManualScrolled ->
                    ( model
                    , Task.attempt ( ChatMsgsViewEvent << Chat.GotViewport ) <|
                        Dom.getViewportOf Views.Chat.msgsViewHtmlId
                    )

                Chat.GotViewport result ->
                    case result of
                        Err err ->
                            ( model, port_DebugLog <| Debug.toString err )

                        Ok viewport ->
                            let
                                logViewport =
                                    port_DebugLog <| String.fromFloat viewport.viewport.y ++ ", "
                                                  ++ String.fromFloat viewport.viewport.height ++ ", "
                                                  ++ String.fromFloat viewport.scene.height

                                hasManualScrolledUp =
                                    Utils.hasManualScrolledUp viewport Chat.autoScrollMargin
                            in
                            ( { model | chatStatus =
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
                    ( { model | chatStatus =
                        { chatStatus | shouldHintNewMsg = False
                                     , hasManualScrolledUp = False
                        }
                      }
                    , snapScrollChatMsgsView
                    )

        OnChatInputFocal isFocused ->
            ( { model | chatStatus =
                { chatStatus | isInputFocused = isFocused }
              }
            , Cmd.none
            )

        OnWindowResized ->
            ( model, getViewportCmd )

        OnKeyDown key ->
            case key of
                "Enter" ->
                    ( model
                    , if ( not model.isShiftHeld ) && model.chatStatus.isInputFocused then
                        sendChatMsg model.chatStatus.input
                      else
                        Cmd.none
                    )

                "Shift" ->
                    ( { model | isShiftHeld = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnKeyUp key ->
            case key of
                "Shift" ->
                    ( { model | isShiftHeld = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Nop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Hideout"
    , body =
        [ Element.layout
            [ Background.color bgColor
            , Font.color white
            ]
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , windowPadding model.viewport.viewport.width
                ]
                (case model.route of
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
                                , Element.link
                                    []
                                    { url = aboutUrl
                                    , label = Element.text "> How does it work?"
                                    }
                                ]
                            ]

                    About ->
                        Element.text "About page"

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
