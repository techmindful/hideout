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
import Letter exposing
    ( letterMetaJsonDec )
import List.Extra as List
import Route exposing (..)
import String.Extra exposing (unquote)
import Tagged exposing ( tag, untag )
import Task
import Url exposing (Url)
import Url.Parser
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
port port_WsError : ( () -> msg ) -> Sub msg
port port_RecvWsMsg : ( String -> msg ) -> Sub msg
port port_DebugLog : String -> Cmd msg


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.batch
        [ port_WsReady <| ChatElmMsg << Chat.OnWsReady
        , port_WsError ( \_ -> ChatElmMsg Chat.OnWsError )
        , port_RecvWsMsg <| ChatElmMsg << Chat.OnWsMsg

        , Browser.Events.onResize ( \_ _ -> OnWindowResized )

        , Browser.Events.onKeyDown <|
            JDec.map OnKeyDown <| JDec.field "key" JDec.string

        , Browser.Events.onKeyUp <|
            JDec.map OnKeyUp <| JDec.field "key" JDec.string

        , Browser.Events.onVisibilityChange OnVisibilityChange
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
      , windowVisibility = Browser.Events.Visible -- Assume visible..

      , aboutPageModel = Views.About.init |> updateAboutPageModelWithRoute route

      , joinChatInput = ""

      , letterRawInput = { body = "", maxReadCount = "1" }
      , letterPersistInput = True
      , letterStatus = { read = Letter.Init, write = Letter.NotSent }

      , spawnDispChatResp = NotSpawned_Disp
      , spawnPersistChatResp = NotSpawned_Persist

      , dispChatMaxJoinCountInput = "2"
      , persistChatMaxJoinCountInput = "2"
      , chatStatus = 
          case route of
              Chat chatIdStr ->
                  Chat.OpeningWs <| tag chatIdStr
                  
              _ ->
                  Chat.NotChatting

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
            let
                route = getRoute url
            in
            case route of
                Chat chatIdStr ->
                    ( Normal { model |
                        route = route
                      , chatStatus = Chat.OpeningWs <| tag chatIdStr
                      }
                    , port_InitWs chatIdStr
                    )

                ReadLetter letterIdStr ->
                    ( Normal { model | route = route }
                    , getLetterReq letterIdStr
                    )

                About _ ->
                    ( Normal { model |
                        route = route
                      , aboutPageModel =
                          updateAboutPageModelWithRoute
                            route
                            model.aboutPageModel
                      }
                    , Cmd.none
                    )

                _ ->
                    ( Normal { model | route = route }
                    , Cmd.none
                    )

        GotViewport viewport ->
            ( Normal { model | viewport = viewport }, Cmd.none )

        AboutPageMsg aboutPageMsg ->
            ( Normal { model | aboutPageModel = Views.About.update aboutPageMsg model.aboutPageModel }
            , Cmd.none
            )

        GotReadLetterResp result ->
            ( Normal { model | letterStatus =
                { letterStatus | read = Letter.Got result }
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
            ( Normal { model | letterRawInput =
                { letterRawInput | body = str }
              }
            , Cmd.none
            )

        LetterMaxReadCountInput str ->
            ( Normal { model | letterRawInput =
                { letterRawInput | maxReadCount = str }
              }
            , Cmd.none
            )

        LetterPersistInput input ->
            ( Normal { model | letterPersistInput = input }, Cmd.none )

        LetterSend ->
            case Letter.validateInput letterRawInput of
                Err _ -> ( Normal model, Cmd.none )
                Ok goodInput ->
                    ( Normal { model |
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
                                Normal { model | letterStatus =
                                    { letterStatus | write = Letter.GotResp <| Err err }
                                }

                            Ok letterId ->
                                Normal { model | letterStatus =
                                    { letterStatus | write =
                                        Letter.GotResp <| Ok
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
            ( Normal { model | dispChatMaxJoinCountInput = str }
            , Cmd.none
            )

        SpawnDispChat ->
            case strToPosIntInput model.dispChatMaxJoinCountInput of
                Bad  _ -> ( Normal model, Cmd.none )
                Good posInt ->
                    ( Normal { model | spawnDispChatResp = Waiting_Disp }
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
                Err err ->
                    ( Normal { model | spawnDispChatResp = GotError_Disp err }
                    , Cmd.none
                    )

                Ok chatIdStr ->
                    ( Normal { model | spawnDispChatResp = GotChatId chatIdStr }
                    , Nav.pushUrl model.navKey <| frontendChatUrl <| unquote chatIdStr
                    )


        PersistChatMaxJoinCountInput str ->
            ( Normal { model | persistChatMaxJoinCountInput = str }
            , Cmd.none
            )

        SpawnPersistChat ->
            case strToPosIntInput model.persistChatMaxJoinCountInput of
                Bad _ -> ( Normal model, Cmd.none )
                Good posInt ->
                    ( Normal { model | spawnPersistChatResp = Waiting_Persist }
                    , Http.request
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
            ( Normal
                { model |
                    spawnPersistChatResp =
                        case result of
                            Err err ->
                                GotError_Persist err
                                
                            Ok letterId ->
                                GotLetterId letterId
                }
            , Cmd.none
            )


        ChatElmMsg chatElmMsg ->
            let
                ( status, cmd ) =
                    Views.Chat.update chatElmMsg model.chatStatus model.windowVisibility model.navKey
            in
            ( Normal { model | chatStatus = status }
            , Cmd.map ChatElmMsg cmd
            )

        OnWindowResized ->
            ( Normal model, getViewportCmd )

        OnVisibilityChange visibility ->
            ( Normal { model | windowVisibility = visibility }
            , Cmd.none
            )

        OnKeyDown key ->
            case model.route of
                Chat _ ->
                    let
                        ( status, cmd ) = Views.Chat.handleKeyDown model.chatStatus key
                    in
                    ( Normal { model | chatStatus = status }
                    , Cmd.map ChatElmMsg cmd
                    )

                _ ->
                    ( Normal model, Cmd.none )

        OnKeyUp key ->
            case model.route of
                Chat _ ->
                    let
                        ( status, cmd ) = Views.Chat.handleKeyUp model.chatStatus key
                    in
                    ( Normal { model | chatStatus = status }
                    , Cmd.map ChatElmMsg cmd
                    )

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

                    About sectionFromRoute ->
                        let
                            aboutPageView =
                                Views.About.view
                                    model.viewport.viewport.width
                                    model.aboutPageModel
                        in
                        Element.map AboutPageMsg aboutPageView

                    ReadLetter id -> Views.ReadLetter.view model
                            
                    WriteLetter -> Views.WriteLetter.view model

                    Chat chatId ->
                        Element.map
                            ChatElmMsg
                            ( Views.Chat.view model.chatStatus model.viewport.viewport.width )

                    ConfigChat -> Views.ConfigChat.view model

                    NotFound ->
                        Element.text "404"
                )
            )
        ]
    }


{-| If route is at about page and specifies a section to show,
then only show that section. Otherwise, keep original sections to show.
-}
updateAboutPageModelWithRoute : Route -> Views.About.Model -> Views.About.Model
updateAboutPageModelWithRoute route model =
    let
        previousSection = model.sectionToShow
    in
    { model |
        sectionToShow =
            case route of
                About section ->
                    case section of
                        Views.About.None -> previousSection
                        _ -> section

                _ -> previousSection
    }


getLetterReq : String -> Cmd Msg
getLetterReq letterId =
    Http.get
        { url = backendReadLetterUrl ++ "/" ++ letterId
        , expect = Http.expectJson GotReadLetterResp letterMetaJsonDec
        }


getViewportCmd : Cmd Msg
getViewportCmd = Task.perform GotViewport Dom.getViewport


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
