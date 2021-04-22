module Views.ReadLetter exposing ( view )

import CoreTypes exposing ( Model, Msg(..) )
import Common.Contents exposing
    ( plainPara )
import Common.Styles exposing
    ( windowPaddingPx )
import Element exposing ( Element )
import Element.Border as Border
import Letter
import UserStatus exposing (..)
import Utils.Markdown
import Utils.Utils as Utils


view : Model -> Element Msg
view model =
    let
        windowWidth = model.viewport.viewport.width

        maxWidthPx =
            round <| windowWidth - 2 * ( windowPaddingPx windowWidth )

        intro =
            Element.column
                [ Element.width <| Element.maximum 750 Element.fill ]
                [ plainPara "You received a Hideout letter!"
                , Element.paragraph
                    [ Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 } ]
                    [ Element.text
                        """
                        Hideout is a service for private messaging. This letter can be read a strictly limited times. So please don't refresh or reopen this letter. It prevents other recipients from accessing it!
                        """
                    ]
                ]
 
    in
    Element.column
        [ Element.width <| Element.maximum maxWidthPx Element.fill ]
        [ case model.letterStatus.read of
            Letter.Waiting ->
                plainPara "Waiting for the letter from server.."

            Letter.Got result ->
                case result of
                    Err err ->
                        plainPara <| Debug.toString err

                    Ok letterMeta ->
                        Element.column
                            [ Element.width Element.fill ]
                            [ intro
                            -- Read count info
                            , Element.textColumn
                                [ Element.paddingXY 0 40
                                , Element.spacingXY 0 5
                                ]
                                [ Element.paragraph
                                    []
                                    [ Element.text "This letter is being read for the "
                                    , Element.text <| Utils.intToOrdStr letterMeta.readCount
                                    , Element.text " time."
                                    ]
                                , Element.paragraph
                                    []
                                    [ Element.text "It can be read at most "
                                    , Element.text <| String.fromInt letterMeta.letter.maxReadCount
                                    , Element.text " times."
                                    ]
                                ]
                            , Element.paragraph
                                [ Element.paddingEach { bottom = 20, top = 0, left = 0, right = 0 } ]
                                [ Element.text "Below is the letter." ]
                            -- Letter
                            , Element.column
                                [ Element.width Element.fill
                                , Element.paddingEach { top = 60, bottom = 0, left = 0, right = 0 }
                                , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
                                ] <|
                                Utils.Markdown.render letterMeta.letter.body
                            ]

            _ -> plainPara "Error: Unaddressed UserStatus case!"
        ]

