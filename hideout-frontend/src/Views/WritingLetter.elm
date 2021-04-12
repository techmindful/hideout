module Views.WritingLetter exposing ( view )


import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Common.Colors exposing (..)
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
import Utils.Utils as Utils exposing (..)


view : Model -> Element Msg
view model =
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

        preview =
            Element.el
                [ Element.width Element.fill
                , Element.alignTop
                ] <|
                Utils.Markdown.render model.letterInput

    in
    Element.column
        [ Element.width Element.fill ]
        [ instruction
        , Element.row
            [ Element.width Element.fill ]
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
