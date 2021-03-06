module Views.About exposing
    ( Model
    , Msg
    , Section(..)
    , init
    , sectionToUrl
    , update
    , urlFragToSection
    , view
    )

import AssocList exposing (Dict)
import Common.Colors exposing (green)
import Common.Contents
    exposing
        ( italicText
        , link
        , newTabLink
        , plainPara
        , sizedPara
        , sizedText
        , spacedPara
        , underlinedText
        )
import Common.Styles exposing (paraSpacing)
import Common.Urls
    exposing
        ( aboutUrl
        , rootUrl
        )
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import List
import List.Extra as List
import Set exposing (Set)
import Url.Builder


type Section
    = What_Is_Hideout
    | Why_Privacy
    | Use_Cases
    | Threat_Model
    | How_Private
    | Persist_Chat
    | Troubleshooting
    | Hideout_Vs_Apps
    | Feature_Richness
    | Self_Hosting
    | Func_Prog
    | None


urlFragAndSectionAssoc : Dict String Section
urlFragAndSectionAssoc =
    AssocList.fromList
        [ ( "what-is-hideout", What_Is_Hideout )
        , ( "why-privacy", Why_Privacy )
        , ( "use-cases", Use_Cases )
        , ( "threat-model", Threat_Model )
        , ( "how-private", How_Private )
        , ( "persist-chat", Persist_Chat )
        , ( "troubleshooting", Troubleshooting )
        , ( "hideout-vs-apps", Hideout_Vs_Apps )
        , ( "feature-richness", Feature_Richness )
        , ( "self-hosting", Self_Hosting )
        , ( "func-prog", Func_Prog )
        ]


urlFragToSection : Maybe String -> Section
urlFragToSection maybeStr =
    case maybeStr of
        Nothing ->
            None

        Just str ->
            AssocList.get str urlFragAndSectionAssoc
                |> Maybe.withDefault None


sectionToUrl : Section -> String
sectionToUrl section =
    let
        urlFrag : String
        urlFrag =
            urlFragAndSectionAssoc
                |> AssocList.toList
                |> List.filter (\( _, section_ ) -> section == section_)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault "[Error: Section missing]"
    in
    Url.Builder.custom Url.Builder.Relative [ aboutUrl ] [] (Just urlFrag)


type Title
    = Title (Element Msg)


type Body
    = Body (Element Msg)


type alias Model =
    { sectionToShow : Section }


type Msg
    = OnExpandSection Section
    | OnCollapseSection


init : Model
init =
    { sectionToShow = What_Is_Hideout }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnExpandSection section ->
            { model | sectionToShow = section }

        OnCollapseSection ->
            { model | sectionToShow = None }


view : Float -> Model -> Element Msg
view screenWidth model =
    let
        sectionViews =
            List.map
                (\sectionView -> sectionView model)
                [ what_is_hideout
                , why_privacy
                , use_cases
                , threat_model
                , how_private
                , persist_chat
                , troubleshooting
                , hideout_vs_apps
                , feature_richness
                , self_hosting
                , func_prog
                ]
    in
    Element.column
        [ Element.width <| Element.maximum 1000 Element.fill
        , Element.height Element.fill
        , sectionSpacing
        ]
    <|
        [ link rootUrl "<< Hideout Home" ]
            ++ sectionViews
            ++ [ Element.newTabLink
                    [ Element.alignBottom
                    , Font.size 24
                    , Font.underline
                    ]
                    { url = "https://github.com/techmindful/hideout"
                    , label = Element.text "GitHub"
                    }
               ]


what_is_hideout : Model -> Element Msg
what_is_hideout model =
    let
        body =
            Body <|
                Element.column
                    []
                    [ plainPara "Hideout is a self-hosted service that makes private, persistent chat rooms possible. It requires no installation or account creation. Unlike disposable chat, a Hideout room preserves privacy without expiring."
                    , Element.paragraph
                        [ Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 } ]
                        [ Element.text "Here's a video demo:" ]
                    , Element.el
                        [ Element.paddingEach { top = 30, bottom = 0, left = 0, right = 0 }
                        ]
                      <|
                        Element.html <|
                            Html.video
                                [ Html.Attributes.controls True
                                , Html.Attributes.width 520
                                , Html.Attributes.height 292
                                ]
                                [ Html.source
                                    [ Html.Attributes.src "/static/videos/intro.mp4"
                                    , Html.Attributes.type_
                                        "video/mp4"
                                    ]
                                    []
                                ]
                    ]
    in
    mkSection What_Is_Hideout "What is Hideout?" body model


why_privacy : Model -> Element Msg
why_privacy model =
    let
        body =
            Body <|
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


use_cases : Model -> Element Msg
use_cases model =
    let
        body =
            Body <|
                Element.column
                    [ paraSpacing ]
                    [ plainPara
                        """
                    1. Hideout is useful when you want to have a private conversation with your friends, but you are currently using a service that violates user's privacy, e.g. Facebook Messenger, Discord, Gmail, and so on. By using Hideout, the conversation is simply moved away from the unprivate service, leaving it nothing to collect and spy on.
                    """
                    , Element.paragraph
                        [ Font.color green ]
                        [ Element.text
                            """
                        2. Hideout is particularly useful if you want a private chat room that can be bookmarked and accessed one-click from the browser. The persistent chat section below explains how it's implemented.
                        """
                        ]
                    , plainPara
                        """
                    3. Hideout is useful if your friends are scattered across multiple messaging apps. Someone on Snapchat can't talk to someone on Signal. But they can convene in a Hideout chat room.
                    """
                    ]
    in
    mkSection Use_Cases "Use Cases" body model


threat_model : Model -> Element Msg
threat_model model =
    let
        body =
            Body <|
                Element.column
                    [ paraSpacing ]
                    [ plainPara
                        """
                    Hideout assumes your communication on the unprivate service isn't compromised. If you want a private conversation without it being logged by the unprivate service, Hideout can help. If your communication on the unprivate service is already being actively monitored and tampered with, there's nothing you can do except establishing a new private communication channel with your friends, preferably in person.
                    """
                    ]
    in
    mkSection Threat_Model "Threat Model" body model


how_private : Model -> Element Msg
how_private model =
    let
        body =
            Body <|
                Element.column
                    [ paraSpacing ]
                    [ plainPara
                        """
                    - Hideout is designed to be self-hosted. The idea is that the more privacy-aware and tech-savvy person among a friend group will host the server, to use with their friends. So trust is already a given. The user trusts the server as much as they trust the person hosting.
                    """
                    , plainPara
                        """
                    - Although Hideout's key feature is a persistent private chat room, it's based on the idea of disposable messages. Disposable letters can't be read after a max read limit is reached. A disposable chat room can't be joined after the max join limit is reached. The entrance to a persistent chat room can't be accessed after the max access limit is reached. This "access-based" approach gives a stronger guarantee of privacy than the "time-based" approach. If a message or chat room is set to be deleted after 15 minutes, nothing stops it from being viewed by unwanted parties at the 14th minute.
                    """
                    ]
    in
    mkSection How_Private "How is Hideout private?" body model


troubleshooting : Model -> Element Msg
troubleshooting model =
    let
        body =
            Body <|
                Element.column
                    [ paraSpacing ]
                    [ plainPara
                        """ 
                    If you received a link to a Hideout letter or chat, but it tells you that the maximum number of time it can be accessed is reached, then maybe some of the intended participants reloaded the letter page or rejoined the chat. If it's made certain that nobody is accessing multiple times, then the grim reality is probably that the communication among your friends is being spied on, and you all should move to secure messaging apps*.
                    """
                    , sizedPara 16
                        """
                    * Actually, it doesn't necessarily mean that it's the unprivate apps like Facebook Messenger and Discord who's the spying adversary. Unprivate operating systems like Windows 10 can be the spying one too. Another possibility is that someone's computer is hacked, and moving to a secure messaging app only obscures the hack. Overall, one should follow good security and privacy practices.
                    """
                    ]
    in
    mkSection Troubleshooting "Troubleshooting" body model


hideout_vs_apps : Model -> Element Msg
hideout_vs_apps model =
    let
        body =
            Body <|
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
                                That's when I started to work on Hideout. A chat service that requires no sign up, and no installation. You can create a chat room and send it to your contacts on unprivate platforms. But the conversation will remain private.
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


feature_richness : Model -> Element Msg
feature_richness model =
    let
        body =
            Body <|
                plainPara
                    """
                Hideout plans to be as feature-rich as possible. Emojis is already here. It just needs some polishing for ease of use. File-sharing and voice/vidoe chat are on the roadmap.
                """
    in
    mkSection Feature_Richness "Feature-richness" body model


persist_chat : Model -> Element Msg
persist_chat model =
    let
        body =
            Body <|
                Element.column
                    [ paraSpacing ]
                    [ plainPara
                        """
                    Persistent chat rooms is a simple yet powerful idea. It's a private chat room that doesn't need to expire. By bookmarking it in the browser, the participants can keep going back to the room, without having to create a new room every time they talk. Hideout achieves this in a very simple way.
                    """
                    , plainPara
                        """
                    Imagine you have already created a disposable chat room and you are chatting with your friends. Everything you say in this room is private. If you go ahead and create a second chat room, you can share the link of this new room privately with your friends. Since only you and your friends know about this new room, it doesn't have to expire. Hideout just automates this process.
                    """
                    , plainPara
                        """
                    Here are the details of how it's automated. A person creates a persistent chat room, and sets the number of participants to 4, for example. This makes Hideout generate a chat room that can be joined infinitely on the server. Then, Hideout generates an "entrance", which contains the room ID. The entrance can only be accessed 4 times. The person shares the link to this entrance (not the chat!) to their 3 friends. The 4 of them each opens the entrance, and joins the room from there. Hideout deletes the entrance after all 4 people have accessed it. So nothing else can get the room ID. But the 4 people now have a persistent chat room that they can always go back into.
                    """
                    , plainPara
                        """
                    Besides convenience, persistent chat is of great value for people who are in situations where it's improper to repeatedly create and share disposable chat room links, as doing so draws unwanted attention. In some countries, you can create and send 1000 room links on Discord every day, without the authority paying you a visit. But that's not the same in a lot of other places on the planet.
                    """
                    ]
    in
    mkSection Persist_Chat "Persistent Chat: Hideout's own invention" body model


self_hosting : Model -> Element Msg
self_hosting model =
    let
        body =
            Body <|
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
                        However, "self-hosting" has a more strict meaning here. Ideally, the server should be run on a device that the person 
                        """
                        , underlinedText "physically owns"
                        , Element.text
                            """
                        . To protect the server operator's IP address, the server should be run behind a VPN. I've experimented and confirmed that it's very practical. 
                        """
                        , newTabLink "https://mullvad.net" "Mullvad VPN"
                        , Element.text
                            """
                         has an open-source VPN client, and offers the ability of port-forwarding. The Hideout server I ran ended up having a URL followed by a port number, like https://www.myhideout.com:12345. Not a big deal since the URL can be bookmarked in the browser. Currently the only inconvenience is that Mullvad's port-forwarding is under development. I couldn't get more than 1 port forwarded. So I couldn't really SSH into my server at the same time.
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


func_prog : Model -> Element Msg
func_prog model =
    let
        body =
            Body <|
                Element.column
                    [ paraSpacing ]
                    [ Element.paragraph
                        []
                        [ Element.text
                            """
                        Hideout is built with 2 functional programming languages: Haskell and Elm. Functional programming is a programming paradigm that deserves a lot more attention and 
                        """
                        , italicText "should"
                        , Element.text
                            """
                         be the future. In brief, it encourages separation of side-effects, and type safety, which eliminates classes of bugs, while making refactoring and testing a charm, instead of a pain. It's also the first paradigm where I find myself actively seeking to learn more, because of its deep yet playful nature. The ultimate goal of functional programming is "If it compiles, it works." So if you prefer to solve compile errors, rather than wrestle with runtime bugs, you'll like functional programming.
                        """
                        ]
                    , Element.paragraph
                        []
                        [ Element.text "I found "
                        , newTabLink "http://learnyouahaskell.com/chapters" "Learn You a Haskell for Great Good"
                        , Element.text
                            """
                         to be a great tutorial to get into functional programming with Haskell. Too bad it's non-HTTPS, so it's insecure to buy a copy with credit card. You can still read it for free though. Alternatively, 
                        """
                        , newTabLink "https://guide.elm-lang.org/" "Elm"
                        , Element.text
                            """
                         is famous for being easy to learn.
                        """
                        ]
                    ]
    in
    mkSection Func_Prog "Functional Programming" body model


titleFontStyle : List (Element.Attribute msg)
titleFontStyle =
    [ Font.size 32 ]


mkSection : Section -> String -> Body -> Model -> Element Msg
mkSection section titleStr (Body body) model =
    let
        title =
            Input.button
                titleFontStyle
                { onPress =
                    Just <|
                        if section == model.sectionToShow then
                            OnCollapseSection

                        else
                            OnExpandSection section
                , label = plainPara titleStr
                }
    in
    Element.column
        [ Element.spacingXY 0 20 ]
        -- Spacing between title and body
        [ title
        , if section == model.sectionToShow then
            body

          else
            Element.none
        ]


sectionSpacing : Element.Attribute msg
sectionSpacing =
    Element.spacingXY 0 60


lineSpacing : Element.Attribute msg
lineSpacing =
    Element.spacingXY 0 10
