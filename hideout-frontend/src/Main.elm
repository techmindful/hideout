port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Chat exposing
    ( ChatId
    , ChatStatus
    , Message
    , MessageBody
    , mkJoinMsg
    , mkMessageMsg
    , msgView
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
import Html
import Http
import Json.Encode as JEnc
import Json.Decode as JDec
import Letter exposing (..)
import Markdown
import Route exposing (..)
import String.Extra exposing (unquote)
import Tagged exposing ( tag, untag )
import Task
import Url exposing (Url)
import Url.Parser
import UserStatus exposing (..)
import Utils.Utils as Utils exposing (..)
import Views.Chat
import Views.WritingLetter


port sendMessage : String -> Cmd msg
port messageReceiver : ( String -> msg ) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver OnWsMsg


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
      , chatStatus = { id = tag "", msgs = [], input = tag "" }
      , tempResp = ""
      }
    , Cmd.batch
        [ getViewportCmd
        , case userStatus of
            ReadLetterReq letterId -> getLetterReq letterId
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

                , -- Send join msg if user lands on chat page,
                  -- And ws is ready.
                  case route of
                    Chat chatIdStr ->
                        if model.isWsReady then
                            sendMessage <| mkJoinMsg
                        else
                            Cmd.none

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

        LetterSend ->
            ( { model | userStatus = SentLetter }
            , Http.request
                { method = "PUT"
                , headers = []
                , url = backendWriteLetterUrl
                , body =
                    Http.jsonBody <|
                        JEnc.object
                            [ ( "body", JEnc.string model.letterInput )
                            , ( "maxReadCount", JEnc.int 1 )
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
            , sendMessage <| mkMessageMsg <| untag model.chatStatus.input
            )

        OnWsMsg str ->
            case str of
                "wsReady" ->
                    ( { model | isWsReady = True }
                    -- If ws is open after user lands on the chat page,
                    -- Send the join msg.
                    , case model.route of
                        Chat chatId -> sendMessage <| mkJoinMsg
                        _ -> Cmd.none
                    )

                _ ->
                    ( { model |
                        chatStatus = { chatStatus |
                            msgs = ( Message <| tag str ) :: chatStatus.msgs 
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
                                , Input.button
                                    []
                                    { onPress = Just NewChat
                                    , label = Element.text "> Start a chat."
                                    }
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
                                            plainPara letterMeta.letter.body

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
