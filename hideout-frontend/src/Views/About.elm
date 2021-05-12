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
    | How_Private
    | None


urlFragToSection : Maybe String -> Section
urlFragToSection maybeStr =
    case maybeStr of
        Nothing  -> None
        Just str ->
            case str of
                "why-privacy" -> Why_Privacy
                "hideout-vs-apps" -> Hideout_Vs_Apps
                "how-private" -> How_Private
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
        , how_private model
        ]


why_privacy : Model -> Element Msg
why_privacy model =
    let
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
    mkSection Why_Privacy "Why Privacy?" body model


hideout_vs_apps : Model -> Element Msg
hideout_vs_apps model =
    let
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
    mkSection
        Hideout_Vs_Apps
        "Why use Hideout, when there are so many secure messaging apps?"
        body
        model


how_private : Model -> Element Msg
how_private model =
    let
        body = Body <|
            Element.column
                [ paraSpacing ]
                [ plainPara
                    """
                    Hideout is supposed to be a communication tool with people who are unwilling to install and sign up for a secure messaging app like Signal. So the links to Hideout letters and chat rooms will be shared on unprivate platforms like Facebook Messenger. Hideout's threat model assumes that the unprivate platform may access that link and spy on its content at any time. It could do so days after the link is sent, or right away.
                    """

                , plainPara
                    """
                    Hideout counters either of these cases in a very simple way: "Access-based" disposable letters and chats. Taking letters for example, the author of a disposable letter specifies how many times it can be read, which equals to the number of intended recipients. If the author wants 3 of their friends to read the letter, and all 3 of them read it, then the Hideout server deletes the letter from its memory and database. Whatever unprivate platform won't be able to access it later. If the unprivate platform accessed the letter's link before all 3 recipients, then at least one recipient is bound to be unable to read the letter. If this happens consistently and it's made certain that nobody is reloading the letter's page, then it's a fairly clear sign of the unprivate platform's spying behavior. This exposes the creepiness of the unprivate platform to all the recipients in an obvious way, hopefully making them reconsider if they should still use the unprivate platform, if not making some news headlines.
                   """ 

                , plainPara
                    """
                    Being "access-based" is one of the places where Hideout differs from other services, many of whom offer "time-based" disposable chats, where a letter or chat room expires after a certain time. This gives no guarantee at all for whether the messages can be viewed by a spying adversary. With Hideout's access-based letters and chats however, either all intended recipients see the messages while nobody else can, or an adversary accesses the messages, immediately blocking one of the intended recipient from accessing, and immediately exposing the spying that's going on.
                    """

                , plainPara
                    """
                    A very important trait of Hideout is that it's desigend to be self-hosted. Learn more about it in the self-hosting section.
                    """
                ]
    in
    mkSection
        How_Private
        "How are chats private on Hideout?"
        body
        model


titleFontStyle : List ( Element.Attribute msg )
titleFontStyle =
    [ Font.size 32 ]


titleOnPress : Bool -> Section -> Msg
titleOnPress isShown section =
    case isShown of
        True  -> OnCollapseSection section
        False -> OnExpandSection   section


mkSection : Section -> String -> Body -> Model -> Element Msg
mkSection section titleStr ( Body body ) model =
    let
        title =
            Input.button
                titleFontStyle
                { onPress = Just <| titleOnPress ( isSectionShown section model ) section
                , label = plainPara titleStr
                }
    in
    Element.column
        [ Element.spacingXY 0 20 ]  -- Spacing between title and body
        [ title
        , if isSectionShown section model then
            body
          else
            Element.none
        ]


isSectionShown : Section -> Model -> Bool
isSectionShown section model =
    List.member section model.sectionsToShow


paraSpacing : Element.Attribute msg
paraSpacing =
    Element.spacingXY 0 20


sectionSpacing : Element.Attribute msg
sectionSpacing =
    Element.spacingXY 0 60


lineSpacing : Element.Attribute msg
lineSpacing =
    Element.spacingXY 0 10

