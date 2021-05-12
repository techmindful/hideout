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
    ( link
    , newTabLink
    , plainPara
    , sizedPara
    , sizedText
    , spacedPara
    , underlinedText
    )
import Common.Urls exposing ( aboutSectionUrl )
import Element exposing ( Element )
import Element.Font as Font
import Element.Input as Input
import List
import List.Extra as List
import Set exposing ( Set )


type Section
    = Why_Privacy
    | How_Does_Hideout_Work
    | Hideout_Vs_Apps
    | Why_Another_Disp
    | Self_Hosting
    | None


urlFragToSection : Maybe String -> Section
urlFragToSection maybeStr =
    case maybeStr of
        Nothing  -> None
        Just str ->
            case str of
                "why-privacy" -> Why_Privacy
                "how-does-hideout-work" -> How_Does_Hideout_Work
                "hideout-vs-apps" -> Hideout_Vs_Apps
                "why-another-disp" -> Why_Another_Disp
                "self-hosting" -> Self_Hosting
                _ -> None


type Title = Title ( Element Msg )


type Body = Body ( Element Msg )


type alias Model =
    { sectionToShow : Section }


type Msg
    = OnExpandSection   Section
    | OnCollapseSection


init : Model
init = { sectionToShow = None }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnExpandSection section ->
            { model | sectionToShow = section }

        OnCollapseSection ->
            { model | sectionToShow = None }


view : Float -> Model -> Element Msg
view screenWidth model =
    Element.column
        [ Element.width <| Element.maximum 1000 Element.fill
        , sectionSpacing
        ]
        [ why_privacy model
        , how_does_hideout_work model
        , hideout_vs_apps model
        , why_another_disp model
        , self_hosting model
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


how_does_hideout_work : Model -> Element Msg
how_does_hideout_work model =
    let
        body = Body <|
            Element.column
                [ paraSpacing ]
                [ plainPara
                    """
                    Hideout makes private conversations possible, for those who are unwilling to move away from unprivate platforms like Facebook Messenger, Discord, Gmail, and install secure messaging apps like Signal, Element, and Wire.
                    """
                ,   plainPara
                    """
                    Hideout provides "access-based" disposable letters and chat rooms. The idea is simple. If you want to send a letter to 3 of your friends, and want to make sure only the 3 of them can see the content, then you can create a Hideout letter, and set the maximum number of times it can be read to 3.
                    """

                , plainPara
                    """
                    This way, either all 3 of your friends read the letter while nothing else can; or a spying adversary reads the letter, which immediately blocks one of the friends from reading it, and immediately exposes its spying behavior to everyone. The exposure hopefully motivates everyone to move to a secure messaging app* like Signal, Element, wire, etc, while potentially making some news headline.
                    """

                , plainPara "Same things for Hideout chats."

                , Element.paragraph
                    []
                    [ underlinedText "Troubleshooting"
                    , Element.text 
                        """
                        : If you received a link to a Hideout letter or chat, but it tells you that the maximum number of time it can be accessed is reached, then maybe some of the intended participants reloaded the letter page or rejoined the chat. If it's made certain that nobody is accessing multiple times, then very grimly, the communication among your friends is being spied on, and you all should move to secure messaging apps*.
                        """
                    ]

                , sizedPara 16
                    """
                    * Actually, it doesn't necessarily mean that it's the unprivate apps like Facebook Messenger and Discord who's the spying adversary. Unprivate operating systems like Windows 10 can be the spying one too. Another possibility is that someone's computer is hacked, and moving to a secure messaging app only obscures the hack. Overall, one should follow good security and privacy practices.
                    """
                ]
    in
    mkSection How_Does_Hideout_Work "How does Hideout work?" body model


hideout_vs_apps : Model -> Element Msg
hideout_vs_apps model =
    let
        body = Body <|
            Element.column
                []
                [ Element.column
                    [ paraSpacing ]
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


why_another_disp : Model -> Element Msg
why_another_disp model =
    let
        body = Body <|
            Element.column
                [ paraSpacing ]
                [ plainPara
                    """
                    Disposable chat is not a new idea. But the concept is in need of a new, good enough implementation. Before I made Hideout, I first looked up on search engines to find a disposable chat service that can serve my needs. But none satisfied me. Several aspects that I see are lacking:
                    """
                , Element.column
                    [ lineSpacing
                    , Element.paddingEach { left = 40, right = 0, top = 0, bottom = 0 }
                    ]
                    [ plainPara
                        """
                        1. Some services aren't open source. It's just a website run by someone I don't know. Why should I trust them?
                        """
                    , plainPara
                        """
                        2. Some services aren't served over HTTPS. Even a script kiddie can spy on, or even modify, my conversations.
                        """
                    , plainPara
                        """
                        3. Some services don't have many features. Hideout plans to implement emojis, file sharing, and voice/video chats. It plans to be as feature-rich as possible.
                        """
                    , Element.paragraph
                        []
                        [ Element.text
                            """
                            4. Some services are "time-based". A spying adversary can easily view the messages before it expires, and there will be no way of knowing. Learn more about Hideout's "access-based" approach in
                            """
                        , link
                            ( aboutSectionUrl "how-does-hideout-work" )
                            "How are chats private on Hideout?"
                        ]
                    ]
                ]
    in
    mkSection Why_Another_Disp "Why make yet another disposable chat service?" body model


self_hosting : Model -> Element Msg
self_hosting model =
    let
        body = Body <|
            Element.column
                [ paraSpacing ]
                [ plainPara
                    """
                    Hideout is designed to be self-hosted. The idea is that the more privacy-minded and tech-savvy person among a friend group can set up a Hideout server, for them and their friends to use. The server is naturally trusted, as the owner is a friend.
                    """

                , Element.paragraph
                    []
                    [ Element.text
                        """
                        However, \"self-hosting\" has a more strict meaning here. Ideally, the server should be run on a device that the person 
                        """
                    , underlinedText "physically owns"
                    , Element.text
                        """
                        . To protect the server operator's IP address, the server should be run behind a VPN. I've experimented and confirmed that it's very practical. Mullvad VPN has an open-source VPN client, and offers the ability of port-forwarding. The Hideout server I ran ended up having a URL followed by a port number, like https://www.myhideout.com:12345. Not a big deal since the URL can be bookmarked in the browser. Currently the only inconvenience is that Mullvad's port-forwarding is under development. I couldn't get more than 1 port forwarded. So I couldn't really SSH into my server etc.
                        """
                    ]

                , Element.paragraph
                    []
                    [ Element.text
                        """
                        A less ideal option is to rent a server from a VPS provider that's reputable for respecting user's privacy. Currently I can only think of 
                        """
                    , newTabLink "https://njal.la" "Njalla"
                    , Element.text ". But I'm open to suggestions on that."
                    ]

                , plainPara
                    """
                    The least ideal, almost unacceptable option is to rent a server from a VPS provider that doesn't necessarily respect user's privacy, like Google Cloud, Amazon AWS, and so on. If you can rent a server there, I don't see why you can't rent a server from Njalla.
                    """

                , plainPara
                    """
                    Hideout isn't designed to be hosted on a server that gives service to a large population. People shouldn't place much trust on servers run by strangers. Futhermore, such a server will probably be spammed a lot, as Hideout does not have account registration.
                    """
                ]
    in
    mkSection Self_Hosting "Hideout is designed to be self-hosted!" body model

                        


titleFontStyle : List ( Element.Attribute msg )
titleFontStyle =
    [ Font.size 32 ]


mkSection : Section -> String -> Body -> Model -> Element Msg
mkSection section titleStr ( Body body ) model =
    let
        title =
            Input.button
                titleFontStyle
                { onPress = Just <|
                    if section == model.sectionToShow then
                        OnCollapseSection
                    else
                        OnExpandSection section
                , label = plainPara titleStr
                }
    in
    Element.column
        [ Element.spacingXY 0 20 ]  -- Spacing between title and body
        [ title
        , if section == model.sectionToShow then
            body
          else
            Element.none
        ]


paraSpacing : Element.Attribute msg
paraSpacing =
    Element.spacingXY 0 20


sectionSpacing : Element.Attribute msg
sectionSpacing =
    Element.spacingXY 0 60


lineSpacing : Element.Attribute msg
lineSpacing =
    Element.spacingXY 0 10

