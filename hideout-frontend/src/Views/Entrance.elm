module Views.Entrance exposing
    ( Model
    , Status(..)
    , view
    )

import Common.Contents exposing
    ( newTabLink
    , plainPara
    , underlinedText
    )
import Common.Styles exposing
    ( buttonStyle
    , paraSpacing
    , widthConstraint
    )
import Common.Urls exposing (..)
import Element exposing ( Element )
import Http
import Utils.Errors exposing ( httpErrToStr )
import Views.About


type Status
    = NotEntering
    | WaitingForRoomId
    | GotError Http.Error
    | GotRoomId String


type alias Model =
    { status : Status }


view : Status -> Element msg
view status =
    Element.el
        [ widthConstraint ] <|
        case status of
            NotEntering ->
                plainPara "Not entering an entrance. Probably a logic error."

            WaitingForRoomId ->
                plainPara "Waiting for the persistent chat room ID from server..."

            GotError err ->
                case err of
                    Http.BadStatus 404 ->
                        Element.paragraph
                            []
                            [ Element.text
                                """
                                You were invited to a Hideout persistent chat room! But the entrance ID isn't found. Learn more about what this implies 
                                """
                            , newTabLink
                                ( Views.About.sectionToUrl Views.About.Troubleshooting )
                                "here"
                            , Element.text "."
                            ]

                    _ ->
                        plainPara <| httpErrToStr err

            GotRoomId roomId ->
                Element.column
                    [ paraSpacing ]
                    [ plainPara
                        """
                        You are invited to a Hideout persistent chat room! After you join, bookmark the webpage. Then you'll have a private chat room with your friends, that is accessible one-click in your browser.
                        """
                    , Element.paragraph
                        []
                        [ Element.text
                            """
                            You can share the link to this entrance page with other friends. 
                            """
                        , underlinedText
                            """
                            But never share the link to the chat room itself on any unprivate platform.
                            """
                        ]
                    , Element.column
                        [ Element.paddingXY 0 40
                        , Element.spacingXY 0 20
                        ]
                        [ Element.newTabLink
                            ( buttonStyle 5 )
                            { url = frontendChatUrl roomId
                            , label = Element.text "Join"
                            }
                        ]
                    ]

