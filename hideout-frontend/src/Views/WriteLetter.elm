module Views.WriteLetter exposing (view)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Common.Colors exposing (..)
import Common.Contents
    exposing
        ( Tabness(..)
        , borderedButton
        , footer
        , plainPara
        , posIntInputHint
        )
import Common.Styles exposing (..)
import Common.Urls exposing (..)
import CoreTypes exposing (..)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Http
import Json.Decode as JDec
import Json.Encode
import Letter exposing (..)
import Route exposing (..)
import String.Extra exposing (unquote)
import Task
import Url exposing (Url)
import Url.Parser
import Utils.Errors exposing (httpErrToStr)
import Utils.Markdown
import Utils.Types
    exposing
        ( PosIntInput(..)
        , Trio(..)
        , posIntInputToStr
        )


view : Model -> Element Msg
view model =
    let
        instruction =
            Element.textColumn
                [ Element.spacingXY 0 20 ]
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

        maxReadCountInput =
            Element.row
                [ Element.paddingEach { top = 40, bottom = 10, left = 0, right = 0 }
                , Element.spacingXY 10 0
                ]
                [ Element.text "This letter can be read "
                , Input.text
                    inlineInputStyle
                    { onChange = LetterMaxReadCountInput
                    , text = model.letterRawInput.maxReadCount
                    , placeholder = Nothing
                    , label = Input.labelHidden ""
                    }
                , Element.text " times."
                ]

        persistInput =
            Input.checkbox
                [ Element.paddingXY 0 20 ]
                { onChange = LetterPersistInput
                , icon = Input.defaultCheckbox
                , checked = model.letterPersistInput
                , label = Input.labelRight [] <| Element.text "Disk Persistence"
                }

        windowWidth =
            model.viewport.viewport.width

        letterInputWidthConstraint =
            Element.width <|
                Element.maximum
                    (round <|
                        (windowWidth - 2 * windowPaddingPx windowWidth - dividerWidth)
                            / 2
                    )
                    Element.fill

        letterInputBox =
            Input.multiline
                [ letterInputWidthConstraint
                , Element.alignTop
                , Element.scrollbarY
                , Background.color bgColor
                ]
                { onChange = LetterInput
                , text = model.letterRawInput.body
                , placeholder = Nothing
                , label = Input.labelAbove [] Element.none
                , spellcheck = False
                }

        preview =
            Element.column
                [ letterInputWidthConstraint
                , Utils.Markdown.viewSpacing
                , Element.alignTop
                ]
            <|
                Utils.Markdown.render model.letterRawInput.body
    in
    Element.column
        [ Element.width Element.fill ]
        [ Element.column
            [ widthConstraint ]
            [ instruction
            , maxReadCountInput
            , posIntInputHint model.letterRawInput.maxReadCount
            , persistInput
            ]
        , Element.row
            [ Element.width Element.fill ]
            [ case model.letterStatus.write of
                Letter.NotSent ->
                    letterInputBox

                Letter.Sent _ ->
                    Element.paragraph
                        [ Element.width Element.fill ]
                        [ Element.text
                            "Letter is sent. Waiting for the letter ID from server..."
                        ]

                Letter.GotError err ->
                    plainPara <| httpErrToStr err

                Letter.GotResp { id, maxReadCount, copyToClipboardResult } ->
                    let
                        letterLink =
                            model.origin
                                ++ (frontendReadLetterUrl <| unquote id)
                    in
                    Element.column
                        ([ Element.width Element.fill ]
                            ++ roundedBorder 10
                        )
                        [ Element.column
                            [ lineSpacing ]
                            [ Element.paragraph
                                []
                                [ Element.text "Your letter can be read "
                                , Element.text <| String.fromInt maxReadCount
                                , Element.text " times, at:"
                                ]
                            , plainPara letterLink
                            ]
                        , Element.column
                            [ Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
                            , Element.spacingXY 0 10
                            ]
                            [ Element.el
                                [ Element.width Element.shrink ]
                                (borderedButton (OnUserSharesLetter letterLink) "Share Letter")
                            , case copyToClipboardResult of
                                Empty ->
                                    Element.none

                                Positive ->
                                    Element.text "Letter link is copied!"

                                Negative ->
                                    Element.text
                                        """
                                    Copying letter link to clipboard failed. Are you using an old browser like IE?
                                    """
                            ]
                        ]
            , divider
            , preview
            ]
        , case model.letterStatus.write of
            Letter.NotSent ->
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
        , footer 200 SameTab
        ]


divider : Element Msg
divider =
    Element.el [ Element.width <| Element.px dividerWidth ] Element.none


dividerWidth =
    100
