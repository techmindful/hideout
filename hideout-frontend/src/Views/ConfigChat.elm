module Views.ConfigChat exposing ( view )

import Common.Colors exposing ( bgColor )
import Common.Contents exposing
    ( borderedButton
    , italicText
    , plainPara
    , posIntInputHint
    , underlinedText
    )
import Common.Styles exposing ( widthConstraint )
import Common.Urls exposing (..)
import CoreTypes exposing ( Model, Msg(..) )
import Element exposing ( Element )
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import String.Extra exposing ( unquote )
import Url.Builder
import UserStatus exposing ( UserStatus(..) )
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

        , case model.userStatus of
           GotPersistChatIdLetter result ->
              case result of
                 Err _ ->
                     Element.text "Error!"

                 Ok letterId ->
                     Element.textColumn
                        [ Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
                        , Element.spacingXY 0 20
                        ]
                        [ Element.paragraph
                            []
                            [ Element.text
                                """
                                A disposable letter that contains the instruction to join the chat has been generated. Share the 
                                """
                            , underlinedText "link"
                            , Element.text
                                """
                                 (not the content) to the letter below with your contacts.
                                """
                            ]
                        , plainPara <|
                            model.origin ++
                            frontendReadLetterUrl ++ "/" ++
                            unquote letterId
                        ]
                   
           _ -> 
               Element.el
                   [ Element.paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ]
                   ( borderedButton SpawnPersistChat "Start a persistent chat!" )
        ]


numParticipantsInput : ( String -> Msg ) -> String -> Element Msg
numParticipantsInput msg input =
    Element.row
        [ Element.paddingXY 0 20 ]
        [ Element.text "Number of participants: "
        , Input.text
            [ Element.width <| Element.px 100
            , Element.height <| Element.maximum 40 Element.fill
            , Background.color bgColor
            ]
            { onChange = msg
            , text = input
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        ]

