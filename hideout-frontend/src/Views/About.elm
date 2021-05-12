module Views.About exposing
    ( Model
    , Msg
    , Section(..)
    , init
    , update
    , urlFragToSection
    , view
    )

import Common.Contents exposing
    ( plainPara
    , sizedPara
    , sizedText
    , spacedPara
    , underlinedText
    )
import Common.Styles exposing
    ( widthConstraint )
import Element exposing ( Element )
import Element.Font as Font
import Element.Input as Input
import List
import List.Extra as List
import Set exposing ( Set )


type Section
    = Why_Privacy
    | Hideout_Vs_Apps
    | None


urlFragToSection : Maybe String -> Section
urlFragToSection maybeStr =
    case maybeStr of
        Nothing  -> None
        Just str ->
            case str of
                "why-privacy" -> Why_Privacy
                "hideout-vs-apps" -> Hideout_Vs_Apps
                _ -> None


type Title = Title ( Element Msg )


type Body = Body ( Element Msg )


type alias Model =
    { sectionsToShow : List Section }


type Msg
    = OnExpandSection   Section
    | OnCollapseSection Section


init : Model
init = { sectionsToShow = [] }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnExpandSection section ->
            { model | sectionsToShow = section :: model.sectionsToShow }

        OnCollapseSection section ->
            { model | sectionsToShow = List.remove section model.sectionsToShow }



view : Float -> Model -> Element Msg
view screenWidth model =
    Element.column
        [ Element.width <| Element.maximum 1000 Element.fill
        , sectionSpacing
        ]
        [ why_privacy model
        , hideout_vs_apps model
        ]


why_privacy : Model -> Element Msg
why_privacy model =
    let
        title = Title <| mkTitle Why_Privacy "Why Privacy?" model

        body = Body <|
            Element.column
                [ Element.spacingXY 0 20 ]
                [ Element.column
                    [ lineSpacing ]
                    [ plainPara "I always like to use this analogy."
                    , plainPara "Imagine two people, Adam and Bob."
                    , plainPara "If Adam knows a lot about Bob, but Bob knows very little about Adam,"
                    , plainPara "Then we say Adam overpowers Bob."
                    , plainPara
                        """
                        If you can agree with that, then you can see the importance of privacy, on a personal scale.
                        """
                    ]
                , Element.column
                    [ lineSpacing ]
                    [ plainPara "But privacy on a personal scale is just a synonym to security."
                    , plainPara "Now replace Adam with governments and corporations,"
                    , plainPara "And replace Bob with the mass population. The people."
                    ]
                ]
    in
    mkSection Why_Privacy title body model


hideout_vs_apps : Model -> Element Msg
hideout_vs_apps model =
    let
        title = Title <|
            mkTitle
                Hideout_Vs_Apps
                "Why use Hideout, when there are so many secure messaging apps?"
                model

        body = Body <|
            Element.column
                []
                [ Element.column
                    [ Element.spacingXY 0 20 ]
                    [ plainPara "The project of Hideout arose from personal needs."
                    , Element.column
                        [ Element.spacingXY 0 20 ]
                        [ plainPara
                            """
                            I've been recommending my family and friends to use secure messaging apps like Signal, Wire, Element and so on. It wasn't easy. But some of them did sign up, installed the software, and started using it. But after a while, I noticed that the graph of my contacts on these apps form a star shape, where I'm the center. I have a lot of contacts on my end. But my friends don't have the initiative to further recommend those apps to their friends. I'm the only contact on their app for each of my friend.
                            """
                        , plainPara
                            """
                            It's getting extremely fast and simple to install a new app, sign up, and start chatting nowadays. But it's still too much efforts for some. It probably won't last too long for my friends, who installed an app and has only me on the contact list. And for the other friends who aren't using secure messaging apps, we are still talking about personal stuff on platforms that don't repsect user privacy.
                            """

                        , Element.paragraph
                            []
                            [ Element.text
                                """
                                That's when I started to work on Hideout. A chat service that requires no sign up, and no installation. You can just bookmark the website, start a disposable chat, and give the link to your contacts, over 
                                """
                            , underlinedText "any"
                            , Element.text
                                """
                                 platform, be it Gmail, Facebook, Snapchat, Discord... Or you can create and bookmark a persistent chat room, and message your contacts from your browser at any time. Hideout offers the guarantee that nobody else can spy on your conversation.
                                """
                            ]
                    ]
                ]
            ]
    in
    mkSection Hideout_Vs_Apps title body model


titleFontStyle : List ( Element.Attribute msg )
titleFontStyle =
    [ Font.size 32 ]


titleOnPress : Bool -> Section -> Msg
titleOnPress isShown section =
    case isShown of
        True  -> OnCollapseSection section
        False -> OnExpandSection   section


mkTitle : Section -> String -> Model -> Element Msg
mkTitle section titleStr model =
    Input.button
        titleFontStyle
        { onPress = Just <| titleOnPress ( isSectionShown section model ) section
        , label = Element.text titleStr
        }


mkSection : Section -> Title -> Body -> Model -> Element Msg
mkSection section ( Title title ) ( Body body ) model =
    Element.column
        [ Element.spacingXY 0 20 ]
        [ title
        , if isSectionShown section model then
            body
          else
            Element.none
        ]


isSectionShown : Section -> Model -> Bool
isSectionShown section model =
    List.member section model.sectionsToShow


sectionSpacing : Element.Attribute msg
sectionSpacing =
    Element.spacingXY 0 60


lineSpacing : Element.Attribute msg
lineSpacing =
    Element.spacingXY 0 10

