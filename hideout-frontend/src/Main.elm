port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Chat exposing (..)
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
import Views.WritingLetter


port sendMessage : String -> Cmd msg
port messageReceiver : ( String -> msg ) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver MessageRecv


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =

    let route = getRoute url
        userStatus = routeToInitUserStatus route

        getViewportCmd = Task.attempt GotViewport Dom.getViewport
    in
    ( { route = route
      , viewport = Err <| Dom.NotFound "DOM viewport data isn't initialized yet."
      , navKey = navKey
      , userStatus = userStatus
      , letterInput = ""
      , chatStatus = { id = tag "", input = tag "" }
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
update msg model =
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
            , case userStatus of
                ReadLetterReq letterId -> getLetterReq letterId
                _ -> Cmd.none
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
                        , input = tag ""
                        }
                      }
                    , Nav.pushUrl model.navKey <| chatUrl <| unquote chatId
                    )

        MessageInput str ->
            ( { model | chatStatus =
                { id = model.chatStatus.id
                , input = tag str
                }
              }
            , Cmd.none
            )

        MessageSend ->
            ( model
            --, Http.request
            --    { method = "PUT"
            --    , headers = []
            --    , url = sendMessageUrl <| untag model.chatStatus.id
            --    , body = Http.jsonBody <|
            --        JEnc.object
            --            [ ( "body", JEnc.string <| untag model.chatStatus.input ) ]
            --    , expect = Http.expectWhatever GotMessageSendResp
            --    , timeout = Nothing
            --    , tracker = Nothing
            --    }
            , sendMessage <| untag model.chatStatus.input
            )

        MessageRecv str ->
            ( { model | tempResp = str }
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

                    Chat chatId ->
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
                                    ]
                                    [ plainPara model.tempResp
                                    ]
                                , Input.multiline
                                    [ Background.color bgColor
                                    , Element.height <| Element.px 200
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
