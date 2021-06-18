module Views.ConfigChat exposing (view)

import Common.Colors exposing (bgColor, red)
import Common.Contents
    exposing
        ( Tabness(..)
        , borderedButton
        , footer
        , italicText
        , link
        , plainPara
        , posIntInputHint
        , underlinedText
        )
import Common.Styles
    exposing
        ( buttonStyle
        , inlineInputStyle
        , widthConstraint
        )
import Common.Urls exposing (..)
import CoreTypes
    exposing
        ( Model
        , Msg(..)
        , SpawnDispChatResp(..)
        , SpawnPersistChatResp(..)
        )
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import String.Extra exposing (unquote)
import Url.Builder
import Utils.Types
    exposing
        ( PosIntInput
        , Trio(..)
        , posIntInputToStr
        )
import Views.About


view : Model -> Element Msg
view model =
    Element.column
        [ widthConstraint ]
        [ Element.paragraph
            []
            [ italicText "Persistent"
            , Element.text
                """
                 chat is Hideout's own invention. It's a private chat room between you and your friends, that can be accessed one-click from your browser. Learn more 
                """
            , link (Views.About.sectionToUrl Views.About.Persist_Chat) "here"
            , Element.text "."
            ]
        , numParticipantsInput PersistChatMaxJoinCountInput model.persistChatMaxJoinCountInput
        , posIntInputHint model.persistChatMaxJoinCountInput
        , Element.el
            [ Element.paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ]
            (borderedButton SpawnPersistChat "Start a persistent chat!")
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

            GotEntranceId { entranceId, copyToClipboardResult } ->
                let
                    entranceLink =
                        model.origin
                            ++ (frontendEntranceUrl <| unquote entranceId)

                    shareEntranceButton =
                        borderedButton (OnUserSharesEntrance entranceLink) "Share Entrance"

                    accessEntranceButton =
                        Element.newTabLink
                            (buttonStyle 5)
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
                    , case copyToClipboardResult of
                        Empty ->
                            Element.none

                        Positive ->
                            Element.text "Entrance link is copied!"

                        Negative ->
                            plainPara
                                """
                                Copying entrance link to clipboard failed. Are you using an old browser like IE?
                                """
                    ]
        , -- Disposable chat
          Element.paragraph
            [ Element.paddingEach { top = 200, bottom = 0, left = 0, right = 0 } ]
            [ Element.text "A "
            , italicText "disposable"
            , Element.text
                """
                 chat is a private chat room that can be only used once. You may have seen similar services somewhere else.
                """
            ]
        , numParticipantsInput DispChatMaxJoinCountInput model.dispChatMaxJoinCountInput
        , posIntInputHint model.dispChatMaxJoinCountInput
        , Element.el
            [ Element.paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ]
            (borderedButton SpawnDispChat "Start a disposable chat!")
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
        , footer 320 SameTab
        ]


numParticipantsInput : (String -> Msg) -> String -> Element Msg
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
