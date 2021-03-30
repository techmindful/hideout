module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Consts.Colors exposing (..)
import Consts.Styles exposing (..)
import CoreTypes exposing (..)
import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html
import Markdown
import Route exposing (..)
import Task
import Url exposing (Url)
import Url.Parser
import Utils.Utils as Utils exposing (..)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { route = getRoute url
      , viewport = Err <| Dom.NotFound "DOM viewport data isn't initialized yet."
      , navKey = navKey
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

        Nop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        viewportWidth =
            Result.withDefault 1920.0 <| Result.map (.scene >> .width) model.viewport
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
                        Element.column
                            [ Element.width Element.fill ]
                            [ Element.paragraph
                                [ Font.size 24 ]
                                [ Element.text "Type away your message below.." ]
                            , Element.row
                                [ Element.width Element.fill ]
                                [ Input.text
                                    [ Element.width Element.fill
                                    , Background.color bgColor
                                    ]
                                    { onChange = \str -> Nop
                                    , text = "test input"
                                    , placeholder = Nothing
                                    , label = Input.labelAbove [] Element.none
                                    }
                                , Element.el [ Element.width <| Element.px 100 ] Element.none
                                , Element.el
                                    [ Element.width Element.fill ]
                                  <|
                                    Element.html <|
                                        Html.div [] <|
                                            Markdown.toHtml Nothing "# Test head\n test *test* **test**"
                                ]
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
