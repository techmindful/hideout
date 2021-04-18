module Views.ReadLetter exposing ( view )

import CoreTypes exposing ( Model, Msg(..) )
import Common.Contents exposing
    ( plainPara )
import Common.Styles exposing
    ( widthConstraint )
import Element exposing ( Element )
import Element.Border as Border
import UserStatus exposing (..)
import Utils.Markdown


view : Model -> Element Msg
view model =
    Element.column
        [ widthConstraint ]
        [ case model.userStatus of
            ReadLetterReq _ ->
                plainPara "Waiting for the letter from server.."

            ReadLetterResp result ->
                case result of
                    Err err ->
                        plainPara <| Debug.toString err

                    Ok letterMeta ->
                        Element.column
                            [ Element.width Element.fill ]
                            [ plainPara "You received a Hideout letter!"
                            , Element.paragraph
                                [ Element.paddingXY 0 20 ]
                                [ Element.text
                                    """
                                    Hideout is a service for private messaging. This letter can be read a strictly limited times. So please don't refresh or reopen this letter. It prevents other recipients from accessing it!
                                    """
                                ]
                            , Element.paragraph
                                [ Element.paddingEach { bottom = 20, top = 0, left = 0, right = 0 }
                                , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                                ]
                                [ Element.text "Below is the letter." ]
                            , Element.column
                                [ Element.width Element.fill
                                , Element.paddingEach { top = 60, bottom = 0, left = 0, right = 0 }
                                ] <|
                                Utils.Markdown.render letterMeta.letter.body
                            ]

            _ -> plainPara "Error: Unaddressed UserStatus case!"
        ]

