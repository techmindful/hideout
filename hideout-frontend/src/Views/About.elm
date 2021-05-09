module Views.About exposing ( view )

import Common.Contents exposing
    ( plainPara
    , sizedPara
    , sizedText
    , underlinedText
    )
import Common.Styles exposing
    ( widthConstraint )
import Element exposing ( Element )

view : Float -> Element msg
view screenWidth =
    Element.column
        [ Element.width <| Element.maximum 1000 Element.fill
        , sectionSpacing
        ]
        [ why_privacy
        , hideout_vs_apps
        ]


why_privacy : Element msg
why_privacy =
    Element.column
        [ Element.spacingXY 0 20 ]
        [ h1 "Why Privacy?"
        , Element.column
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
            [ Element.spacingXY 0 5 ]
            [ plainPara "But privacy on a personal scale is just a synonym to security."
            , plainPara "Now replace Adam with governments and corporations,"
            , plainPara "And replace Bob with the mass population. The people."
            ]
        ]


hideout_vs_apps : Element msg
hideout_vs_apps =
    Element.column
        [ Element.spacingXY 0 20 ]
        [ h1 "Why use Hideout, when there are so many secure messaging apps?"
        , plainPara "The project of Hideout arose from personal needs."
        , plainPara
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


h1 : String -> Element msg
h1 str =
    sizedPara 32 str


sectionSpacing : Element.Attribute msg
sectionSpacing =
    Element.spacingXY 0 60


lineSpacing : Element.Attribute msg
lineSpacing =
    Element.spacingXY 0 5

