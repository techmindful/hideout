port module Main exposing (..)

import Browser
import Browser.Dom as Dom
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
        ]

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =

    let route = getRoute url
        userStatus = routeToInitUserStatus route

        getViewportCmd = Task.attempt GotViewport Dom.getViewport
    in
    ( { route = route
      , viewport = Err <| Dom.NotFound "DOM viewport data isn't initialized yet."
      , navKey = navKey
      , isWsReady = False
      , userStatus = userStatus

      , letterInput = ""
      , letterMaxReadCountInput = Good 1
      , letterPersistInput = True

      , dispChatMaxJoinCountInput = Good 2
      , persistChatMaxJoinCountInput = Good 2
      , chatStatus =
          { chatId = tag ""
          , myUserId = -1
          , msgs = []
          , input = tag ""
          , users = Dict.empty
          , hasManualScrolledUp = False
          , shouldHintNewMsg = False
          }
      , newNameInput = ""

      , tempResp = ""
      }
    , Cmd.batch
        [ getViewportCmd

        , case userStatus of
            ReadLetterReq letterId -> getLetterReq letterId
            _ -> Cmd.none

        , case route of
            Chat chatIdStr -> port_InitWs chatIdStr
            _ -> Cmd.none
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( { chatStatus } as model ) =
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
            , Cmd.batch
                [ case userStatus of
                    ReadLetterReq letterId -> getLetterReq letterId
                    _ -> Cmd.none

                , case route of
                    Chat chatIdStr -> port_InitWs chatIdStr
                    _ -> Cmd.none
                ]
            )

        GotViewport result ->
            case result of
                Err err ->
                    ( { model | viewport = Err err }, Cmd.none )

                Ok viewport ->
                    ( { model | viewport = Ok viewport }, Cmd.none )

        GotReadLetterResp result ->
            ( { model | userStatus = ReadLetterResp result }, Cmd.none )

        LetterInput str ->
            ( { model | letterInput = str }, Cmd.none )

        LetterMaxReadCountInput str ->
            ( { model | letterMaxReadCountInput = strToPosIntInput str }
            , Cmd.none
            )

        LetterPersistInput input ->
            ( { model | letterPersistInput = input }, Cmd.none )

        LetterSend ->
            case model.letterMaxReadCountInput of
                Bad _ -> ( model, Cmd.none )
                Good maxReadCount ->
                    ( { model | userStatus = SentLetter
                              , letterInput = ""
                      }
                    , Http.request
                        { method = "PUT"
                        , headers = []
                        , url = backendWriteLetterUrl
                        , body =
                            Http.jsonBody <|
                                JEnc.object
                                    [ ( "body", JEnc.string model.letterInput )
                                    , ( "maxReadCount", JEnc.int maxReadCount )
                                    , ( "persist", JEnc.bool model.letterPersistInput )
                                    ]
                        , expect = Http.expectString GotLetterSendResp
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

        GotLetterSendResp result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok letterId ->
                    ( { model | userStatus = GotLetterId letterId }, Cmd.none )

        DispChatMaxJoinCountInput str ->
            ( { model | dispChatMaxJoinCountInput = strToPosIntInput str }
            , Cmd.none
            )

        SpawnDispChat ->
            case model.dispChatMaxJoinCountInput of
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
                    , Nav.pushUrl model.navKey <| chatUrl <| unquote chatId
                    )

        PersistChatMaxJoinCountInput str ->
            ( { model | persistChatMaxJoinCountInput = strToPosIntInput str }
            , Cmd.none
            )

        SpawnPersistChat ->
            ( model
            , case model.persistChatMaxJoinCountInput of
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
            ( { model | chatStatus = { chatStatus | input = tag "" } }
            , port_SendWsMsg <| Chat.mkContentMsg model.chatStatus.input
            )

        NewNameInput str ->
            ( { model | newNameInput = str }
            , Cmd.none
            )

        NameChange ->
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
                        Chat.MsgFromServer_ msgFromServer ->
                            let
                                msgFromClient = msgFromServer.msgFromClient
                                senderId = msgFromServer.userId
                                senderName = msgFromServer.username
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
                                isMyMsg = msgFromServer.userId == model.chatStatus.myUserId
                            in
                            ( { model |
                                chatStatus =
                                    { chatStatus |
                                      msgs = chatStatus.msgs ++ [ msgFromServer ]
                                    , users = newUsers
                                    , shouldHintNewMsg =
                                        model.chatStatus.hasManualScrolledUp &&
                                        ( not isMyMsg )

                                    -- Clear input field if it's confirmed that
                                    -- Server has received and broadcasted my last msg.
                                    , input = if isMyMsg then tag ""
                                              else model.chatStatus.input
                                    }
                              }
                            , if not model.chatStatus.hasManualScrolledUp then
                                snapScrollChatMsgsView
                              else
                                Cmd.none
                            )

                        Chat.MsgHistory_ msgHistory ->
                           ( { model |
                              chatStatus =
                                  { chatStatus | msgs  = msgHistory.msgs
                                               , users = msgHistory.users
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

        Nop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        viewportWidth =
            Result.withDefault 1920.0 <| Result.map (.scene >> .width) model.viewport

        viewportHeight =
            Result.withDefault 1080.0 <| Result.map (.scene >> .height) model.viewport
    in
    { title = "Hideout"
    , body =
        [ Element.layout
            [ Background.color bgColor
            , Font.color white
            ]
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 60
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
        ReadLetter letterId -> ReadLetterReq letterId
        WriteLetter -> WritingLetter
        _ -> Other


getLetterReq : String -> Cmd Msg
getLetterReq letterId =
    Http.get
        { url = backendReadLetterUrl ++ "/" ++ letterId
        , expect = Http.expectJson GotReadLetterResp letterMetaJsonDec
        }


snapScrollChatMsgsView : Cmd Msg
snapScrollChatMsgsView =
    Dom.getViewportOf Views.Chat.msgsViewHtmlId
        |> Task.andThen
            ( \ viewport ->
                Dom.setViewportOf
                    Views.Chat.msgsViewHtmlId 0 viewport.scene.height
            )
        |> ( Task.attempt <| ChatMsgsViewEvent << Chat.TriedSnapScroll )


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
