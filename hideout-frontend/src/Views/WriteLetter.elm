module Views.WriteLetter exposing ( view )


import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Common.Colors exposing (..)
import Common.Contents exposing
    ( plainPara
    , posIntInputHint
    )
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
import Route exposing (..)
import String.Extra exposing (unquote)
import Task
import Url exposing (Url)
import Url.Parser
import UserStatus exposing (..)
import Utils.Markdown
import Utils.Types exposing ( PosIntInput(..), posIntInputToStr )


view : Model -> Element Msg
view model =
    let
        instruction =
            Element.textColumn
                [ Element.paddingEach { bottom = 40, top = 0, left = 0, right = 0 }
                , Element.spacingXY 0 20
                ]
                [ plainPara "Type away your letter below. Markdown is supported."
                , plainPara
                    """
                    Send the letter after it's finished. It will be saved to a link, that can be accessed a strictly limited number of times. So don't click that link yourself. Just give it to your contacts.
                    """
                , plainPara
                    """
                    The server deletes the letter after the access limit is reached. So if all of your contacts report that they've read the letter, you'll be sure that nobody else could have read it.
                    """
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
            Element.column
                [ Element.width Element.fill
                , Element.spacing 30
                , Element.alignTop
                ] <|
                Utils.Markdown.render model.letterInput
    in
    Element.column
        [ Element.width Element.fill ]
        [ instruction
        , Element.column
            [ Element.width Element.fill
            , Element.spacingXY 0 20
            ]
            [ Element.row
                [ Element.spacingXY 10 0 ]
                [ Element.text "This letter can be read " 
                , Input.text
                    [ Element.width <| Element.px 100
                    , Element.height <| Element.maximum 40 Element.fill
                    , Background.color bgColor
                    ]
                    { onChange = LetterMaxReadCountInput
                    , text = posIntInputToStr model.letterMaxReadCountInput
                    , placeholder = Nothing
                    , label = Input.labelHidden ""
                    }
                , Element.text " times."
                ]
            , posIntInputHint model.letterMaxReadCountInput

            , Input.checkbox
                []
                { onChange = LetterPersistInput
                , icon = Input.defaultCheckbox
                , checked = model.letterPersistInput
                , label = Input.labelRight [] <| Element.text "Disk Persistence"
                }
            ]

        , Element.row
            [ Element.width Element.fill
            , Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
            ]
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


divider : Element Msg
divider =
    Element.el [ Element.width <| Element.px 100 ] Element.none
