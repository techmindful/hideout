port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Chat
import Common.Colors exposing (..)
import Common.Contents exposing ( posIntInputHint )
import Common.Styles exposing (..)
import Common.Urls exposing (..)
import CoreTypes exposing (..)
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
import Route exposing (..)
import String.Extra exposing (unquote)
import Tagged exposing ( tag, untag )
import Task
import Url exposing (Url)
import Url.Parser
import UserStatus exposing (..)
import Utils.Markdown
import Utils.Types exposing ( PosIntInput(..), posIntInputToStr, strToPosIntInput )
import Utils.Utils as Utils exposing (..)
import Views.Chat
import Views.WritingLetter


port port_InitWs : String -> Cmd msg
port port_WsReady : ( String -> msg ) -> Sub msg
port port_SendWsMsg : String -> Cmd msg
port port_RecvWsMsg : ( String -> msg ) -> Sub msg


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
      , letterMaxReadCountInput = Good 2
      , chatStatus = { id = tag "", msgs = [], input = tag "" }
      , chatMaxJoinCountInput = Good 2
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

        LetterSend ->
            case model.letterMaxReadCountInput of
                Bad _ -> ( model, Cmd.none )
                Good maxReadCount ->
                    ( { model | userStatus = SentLetter }
                    , Http.request
                        { method = "PUT"
                        , headers = []
                        , url = backendWriteLetterUrl
                        , body =
                            Http.jsonBody <|
                                JEnc.object
                                    [ ( "body", JEnc.string model.letterInput )
                                    , ( "maxReadCount", JEnc.int maxReadCount )
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

        NewChat ->
            ( model
            , Http.get
                { url = newChatUrl
                , expect = Http.expectString GotNewChatResp
                }
            )

        ChatMaxJoinCountInput str ->
            ( { model | chatMaxJoinCountInput = strToPosIntInput str }
            , Cmd.none
            )

        GotNewChatResp result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok chatId ->
                    ( { model | chatStatus =
                        { id = tag <| unquote chatId
                        , msgs = []  -- TODO: Display messages before joining.
                        , input = tag ""
                        }
                      }
                    , Nav.pushUrl model.navKey <| chatUrl <| unquote chatId
                    )

        MessageInput str ->
            ( { model | chatStatus = { chatStatus | input = tag str } }
            , Cmd.none
            )

        MessageSend ->
            ( model
            , port_SendWsMsg <| Chat.mkContentMsg <| model.chatStatus.input
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
            case JDec.decodeString Chat.msgFromServerDecoder str of
                Err _ ->
                    ( model, Cmd.none )  -- TODO: Handle error.

                Ok msgFromServer ->
                    ( { model |
                        chatStatus = { chatStatus |
                            msgs = msgFromServer :: chatStatus.msgs 
                        }
                      }
                    , Cmd.none
                    )

        GotMessageSendResp result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok () ->
                    ( model, Cmd.none )

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
    { title = "Disposable Messages"
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
                                    { url = "/about"
                                    , label = Element.text "> How does it work?"
                                    }
                                , Element.link
                                    []
                                    { url = "/write-letter"
                                    , label = Element.text "> Write a letter."
                                    }
                                , Element.column
                                    [ Element.spacingXY 0 10 ]
                                    [ Element.row
                                        []
                                        [ Input.button
                                            []
                                            { onPress = Just NewChat
                                            , label = Element.text "> Start a chat."
                                            }
                                        , Element.row
                                            []
                                            [ Element.text " It can be joined "
                                            , Input.text
                                                [ Element.width <| Element.px 100
                                                , Element.height <| Element.maximum 40 Element.fill
                                                , Background.color bgColor
                                                ]
                                                { onChange = ChatMaxJoinCountInput
                                                , text = posIntInputToStr model.chatMaxJoinCountInput
                                                , placeholder = Nothing
                                                , label = Input.labelHidden ""
                                                }
                                            , Element.text " times."
                                            ]
                                        ]
                                    , posIntInputHint model.chatMaxJoinCountInput
                                    ]
                                ]
                            ]

                    About ->
                        Element.text "About page"

                    ReadLetter id ->
                        Element.column
                            []
                            [ case model.userStatus of
                                ReadLetterReq _ ->
                                    plainPara "Waiting for the letter from server.."

                                ReadLetterResp result ->
                                    case result of
                                        Err err ->
                                            plainPara <| Debug.toString err

                                        Ok letterMeta ->
                                            Utils.Markdown.render letterMeta.letter.body

                                _ -> plainPara "Error: Unaddressed UserStatus case!"
                            ]
                            
                    WriteLetter -> Views.WritingLetter.view model

                    Chat chatId -> Views.Chat.view model

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

main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
