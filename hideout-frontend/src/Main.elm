module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Common.Colors exposing (..)
import Common.Styles exposing (..)
import Common.Urls exposing (..)
import CoreTypes exposing (..)
import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html
import Http
import Json.Encode
import Markdown
import Route exposing (..)
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
    ( { route = getRoute url
      , viewport = Err <| Dom.NotFound "DOM viewport data isn't initialized yet."
      , navKey = navKey
      , userStatus = Other
      , letterInput = ""
      , tempResp = ""
      }
    , Task.attempt GotViewport Dom.getViewport
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
            ( { model | route = getRoute url }, Cmd.none )

        GotViewport result ->
            case result of
                Err err ->
                    ( { model | viewport = Err err }, Cmd.none )

                Ok viewport ->
                    ( { model | viewport = Ok viewport }, Cmd.none )

        LetterInput str ->
            ( { model | letterInput = str }, Cmd.none )

        LetterSend ->
            ( { model | userStatus = SentLetter }
            , Http.request
                { method = "PUT"
                , headers = []
                , url = writeLetterUrl
                , body =
                    Http.jsonBody <|
                        Json.Encode.object [ ( "body", Json.Encode.string model.letterInput ) ]
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
                                    { url = "/letter"
                                    , label = Element.text "> Write a letter."
                                    }
                                ]
                            ]

                    About ->
                        Element.text "About page"

                    ReadLetter id ->
                        Element.text "Read letter"

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

                            divider =
                                Element.el [ Element.width <| Element.px 100 ] Element.none

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
                                [ letterInputBox
                                , divider
                                , preview
                                ]
                            , Element.el
                                [ Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 } ]
                              <|
                                Input.button
                                    (buttonStyle 5)
                                    { onPress = Just LetterSend
                                    , label = Element.text "Send"
                                    }
                            ]

                    NotFound ->
                        Element.text "404"
                )
            )
        ]
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
