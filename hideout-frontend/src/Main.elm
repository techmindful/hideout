module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
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
import Json.Encode
import Json.Decode as JDec
import Letter exposing (..)
import Markdown
import Route exposing (..)
import String.Extra exposing (unquote)
import Task
import Url exposing (Url)
import Url.Parser
import UserStatus exposing (..)
import Utils.Utils as Utils exposing (..)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
                        Json.Encode.object
                            [ ( "body", Json.Encode.string model.letterInput )
                            , ( "maxReads", Json.Encode.int 1 )
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
                            

                    WriteLetter ->
                        let
                            instruction =
                                Element.textColumn
                                    [ Element.paddingEach { bottom = 40, top = 0, left = 0, right = 0 }
                                    , Element.spacingXY 0 20
                                    , Font.size 24
                                    ]
                                    [ plainPara "Type away your message below. Markdown is supported."
                                    , plainPara "Send the letter after it's finished. It will be saved to a link, that can be visited only once. So don't click that link yourself. Just give it to your intended recipient."
                                    , plainPara "After the recipient has opened that link, the server will send the letter to their browser, and then delete it from itself. Therefore, nobody else can read that letter."
                                    ]

                            letterInputBox =
                                Input.multiline
                                    [ Element.width Element.fill
                                    , Element.scrollbarY
                                    , Background.color bgColor
                                    ]
                                    { onChange = LetterInput
                                    , text = model.letterInput
                                    , placeholder = Nothing
                                    , label = Input.labelAbove [] Element.none
                                    , spellcheck = False
                                    }

                            preview =
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.alignTop
                                    ]
                                <|
                                    Element.html <|
                                        Html.div [] <|
                                            Markdown.toHtml Nothing model.letterInput
                        in
                        Element.column
                            [ Element.width Element.fill ]
                            [ instruction
                            , Element.row
                                [ Element.width Element.fill ]
                                [ case model.userStatus of
                                    WritingLetter ->
                                        letterInputBox

                                    SentLetter ->
                                        Element.paragraph
                                            [ Element.width Element.fill ]
                                            [ Element.text
                                                "Letter is sent. Waiting for the letter ID from server..."
                                            ]

                                    GotLetterId letterId ->
                                        Element.textColumn
                                            [ Element.width Element.fill
                                            , lineSpacing
                                            , Element.padding 10
                                            , Border.width 2
                                            , Border.rounded 6
                                            ]
                                            [ plainPara "Your letter can be read (only once) at: "
                                            , plainPara <|
                                                frontendReadLetterUrl ++ "/" ++ unquote letterId
                                            ]

                                    _ ->
                                        plainPara "Error: Unaddressed UserStatus case. Can you report this to the server owner?"
                                , divider
                                , preview
                                ]
                            , case model.userStatus of
                                WritingLetter ->
                                    Element.el
                                        [ Element.paddingEach
                                            { top = 20, bottom = 0, left = 0, right = 0 }
                                        ]
                                    <|
                                        Input.button
                                            (buttonStyle 5)
                                            { onPress = Just LetterSend
                                            , label = Element.text "Send"
                                            }

                                _ ->
                                    Element.none
                            ]

                    NotFound ->
                        Element.text "404"
                )
            )
        ]
    }


divider : Element Msg
divider =
    Element.el [ Element.width <| Element.px 100 ] Element.none


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
