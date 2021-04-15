module Views.ConfigChat exposing ( view )

import Common.Colors exposing ( bgColor )
import Common.Contents exposing
    ( borderedButton
    , italicText
    , posIntInputHint
    )
import Common.Styles exposing ( widthConstraint )
import CoreTypes exposing ( Model, Msg(..) )
import Element exposing ( Element )
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
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
            [ Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 } ]
            ( borderedButton SpawnDispChat "Start a disposable chat!" )
        , Element.paragraph
            [ Element.paddingEach { top = 200, bottom = 0, left = 0, right = 0 } ]
            [ Element.text "A "
            , italicText "persistent"
            , Element.text
                """
                 chat is good for circumventing censorship. You and your friends will first receive a link to a chat room in secret. Then you can bookmark that link, and visit there whenever you need to chat or leave a message.
                """
            ]
        , numParticipantsInput PersistChatMaxJoinCountInput model.persistChatMaxJoinCountInput
        , posIntInputHint model.persistChatMaxJoinCountInput
        ]


numParticipantsInput : ( String -> Msg ) -> PosIntInput -> Element Msg
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
            , text = posIntInputToStr input
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        ]

