module Views.Entrance exposing
    ( Model
    , Status(..)
    , view
    )

import Common.Contents
    exposing
        ( Tabness(..)
        , footer
        , newTabLink
        , plainPara
        , underlinedText
        )
import Common.Styles
    exposing
        ( buttonStyle
        , lineSpacing
        , paraSpacing
        , widthConstraint
        )
import Common.Urls exposing (..)
import Element exposing (Element)
import Http
import Utils.Errors exposing (httpErrToStr)
import Views.About


type Status
    = NotEntering
    | WaitingForRoomId
    | GotError Http.Error


type alias Model =
    { status : Status }


view : Status -> Element msg
view status =
    Element.el
        [ widthConstraint ]
    <|
        case status of
            NotEntering ->
                plainPara "Not entering an entrance. Probably a logic error."

            WaitingForRoomId ->
                plainPara "Waiting for the persistent chat room ID from server..."

            GotError err ->
                case err of
                    Http.BadStatus 404 ->
                        Element.column
                            []
                            [ Element.paragraph
                                []
                                [ Element.text
                                    """
                                    You were invited to a Hideout persistent chat room! But the entrance ID isn't found. Learn more about what this implies 
                                    """
                                , newTabLink
                                    (Views.About.sectionToUrl Views.About.Troubleshooting)
                                    "here"
                                , Element.text "."
                                ]
                            , footer 100 NewTab
                            ]

                    _ ->
                        plainPara <| httpErrToStr err
