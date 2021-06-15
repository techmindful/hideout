module Views.Entrance exposing
    ( Model
    , Status(..)
    , view
    )

import Common.Contents exposing
    ( Tabness(..)
    , footer
    , newTabLink
    , plainPara
    , underlinedText
    )
import Common.Styles exposing
    ( buttonStyle
    , lineSpacing
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
                        Element.column
                            []
                            [ Element.paragraph
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
                            , footer 100 NewTab
                            ]

                    _ ->
                        plainPara <| httpErrToStr err

            GotRoomId roomId ->
                Element.column
                    [ paraSpacing ]
                    [ Element.paragraph
                        []
                        [ Element.text
                            """
                            You are invited to a Hideout persistent chat room! After you enter the room, 
                            """
                        , underlinedText "bookmark the room page"
                        , Element.text
                            """
                            . Then you'll have a private chat room with your friends, that is accessible one-click in your browser.
                            """
                        ]
                    , Element.column
                        [ lineSpacing ]
                        [ plainPara "Two important things:"
                        , plainPara
                            """
                            * Don't share the link to the room itself on any unprivate platform. That will make the room no longer private.
                            """
                        , plainPara
                            """
                            * Don't access this entrance page more than once, like refreshing the page or so on. That will block other participants' access.
                            """
                        ]
                    , Element.column
                        [ Element.paddingXY 0 20
                        , Element.spacingXY 0 20
                        ]
                        [ Element.newTabLink
                            ( buttonStyle 5 )
                            { url = frontendChatUrl roomId
                            , label = Element.text "Enter the chat room"
                            }
                        ]
                    , footer 100 NewTab
                    ]

