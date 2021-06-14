module Views.ConfigChat exposing ( view )

import Common.Colors exposing ( bgColor, red )
import Common.Contents exposing
    ( borderedButton
    , italicText
    , plainPara
    , posIntInputHint
    , underlinedText
    )
import Common.Styles exposing
    ( buttonStyle
    , inlineInputStyle
    , widthConstraint
    )
import Common.Urls exposing (..)
import CoreTypes exposing
    ( Model
    , Msg(..)
    , SpawnDispChatResp(..)
    , SpawnPersistChatResp(..)
    )
import Element exposing ( Element )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import String.Extra exposing ( unquote )
import Url.Builder
import Utils.Types exposing ( PosIntInput, posIntInputToStr )


view : Model -> Element Msg
view model =
    Element.column
        [ widthConstraint ]
        [ Element.paragraph
            []
            [ Element.text "A "
            , italicText "disposable"
            , Element.text
                """
                 chat is a good default option. Only you and your friends can join. The server deletes the chat when the room is empty.
                """
            ]
        , numParticipantsInput DispChatMaxJoinCountInput model.dispChatMaxJoinCountInput
        , posIntInputHint model.dispChatMaxJoinCountInput
        , Element.el
            [ Element.paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ]
            ( borderedButton SpawnDispChat "Start a disposable chat!" )
        , let
            padding =
                Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
          in
          case model.spawnDispChatResp of
            NotSpawned_Disp ->
                Element.none
    
            Waiting_Disp ->
                Element.paragraph
                    [ padding ]
                    [ Element.text "Waiting for response from the server..." ]

            GotError_Disp err ->
                Element.paragraph
                    [ padding
                    , Font.color red
                    ]
                    [ Element.text "Error reaching server!" ]

            GotChatId _ ->
                Element.none


        , Element.paragraph
            [ Element.paddingEach { top = 200, bottom = 0, left = 0, right = 0 } ]
            [ Element.text "A "
            , italicText "persistent"
            , Element.text
                """
                 chat is more convenient. You can bookmark the chat, and send private messages to your contacts without having to make a new chat every time. This especially helps if you need to circumvent censorship. Sharing too many Hideout links may draw unwanted attention.
                """
            ]
        , numParticipantsInput PersistChatMaxJoinCountInput model.persistChatMaxJoinCountInput
        , posIntInputHint model.persistChatMaxJoinCountInput
        , Element.el
             [ Element.paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ]
             ( borderedButton SpawnPersistChat "Start a persistent chat!" )
        , let
            padding =
                Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
          in
          case model.spawnPersistChatResp of
            NotSpawned_Persist -> 
                Element.none

            Waiting_Persist ->
                Element.paragraph
                    [ padding ]
                    [ Element.text "Waiting for response from the server..." ]

            GotError_Persist error ->
                Element.paragraph
                    [ padding
                    , Font.color red
                    ]
                    [ Element.text "Error reaching server!" ]

            GotEntranceId entranceId ->
                let
                    entranceLink =
                        model.origin ++
                        ( frontendEntranceUrl <| unquote entranceId )

                    shareEntranceButton =
                        borderedButton ( OnShareEntrance entranceLink ) "Share Entrance"

                    accessEntranceButton =
                        Element.newTabLink
                            ( buttonStyle 5 )
                            { url = entranceLink
                            , label = Element.text "Access Entrance"
                            }
                in
                Element.column
                    [ padding
                    , Element.spacingXY 0 20
                    ]
                    [ Element.paragraph
                        []
                        [ Element.text
                            """
                            A persistent chat room has been generated. Share the link to the entrance (not the chat room) with your friends.
                            """
                        ]
                    , plainPara entranceLink
                    , Element.row
                        [ Element.spacingXY 40 0 ]
                        [ shareEntranceButton
                        , accessEntranceButton
                        ]
                    ]           
        ]


numParticipantsInput : ( String -> Msg ) -> String -> Element Msg
numParticipantsInput msg input =
    Element.row
        [ Element.paddingXY 0 20 ]
        [ Element.text "Number of participants: "
        , Input.text
            inlineInputStyle
            { onChange = msg
            , text = input
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        ]

