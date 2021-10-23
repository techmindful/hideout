# What is Hideout?
Hideout is a self-hosted service that makes private, persistent chat rooms possible. It requires no installation or account creation. Unlike disposable chat, a Hideout room preserves privacy without expiring.

A video demo can be watched here: https://vimeo.com/564494164. Alternatively, the video is available [here](https://github.com/techmindful/hideout/blob/main/hideout-frontend/static/videos/intro.mp4), or on the about page of a running instance.

See [hosting.md](https://github.com/techmindful/hideout/blob/main/hosting.md) for hosting Hideout, [CONTRIBUTING.md](https://github.com/techmindful/hideout/blob/main/CONTRIBUTING.md) for the contribution guideline, [building.md](https://github.com/techmindful/hideout/blob/main/building.md) for building Hideout from source and developing Hideout.

A demo instance is running at [hideout-demo.com](https://www.hideout-demo.com/). Uptime is expected to be unstable. Do not trust it for your own private conversations. 

The text below is an introduction.

---

# Why Privacy?
I always like to use this analogy.

Imagine two people, Adam and Bob.

If Adam knows a lot about Bob, but Bob knows very little about Adam,

Then we say Adam overpowers Bob.

If you can agree with that, then you can see the importance of privacy, on a personal scale.

But privacy on a personal scale is just a synonym to security.

Now replace Adam with governments and corporations,

And replace Bob with the mass population. The people.

# Use Cases
1. Hideout is useful when you want to have a private conversation with your friends, but you are currently using a service that violates user's privacy, e.g. Facebook Messenger, Discord, Gmail, and so on. By using Hideout, the conversation is simply moved away from the unprivate service, leaving it nothing to collect and spy on.
2. Hideout is particularly useful if you want a private chat room that can be bookmarked and accessed one-click from the browser. The persistent chat section below explains how it's implemented.
3. Hideout is useful if your friends are scattered across multiple messaging apps. Someone on Snapchat can't talk to someone on Signal. But they can convene in a Hideout chat room.

# Threat Model
Hideout assumes your communication on the unprivate service isn't compromised. If you want a private conversation without it being logged by the unprivate service, Hideout can help. If your communication on the unprivate service is already being actively monitored and tampered with, there's nothing you can do except establishing a new private communication channel with your friends, preferably in person.

# How is Hideout private?
- Hideout is designed to be self-hosted. The idea is that the more privacy-aware and tech-savvy person among a friend group will host the server, to use with their friends. So trust is already a given. The user trusts the server as much as they trust the person hosting.
- Although Hideout's key feature is a persistent private chat room, it's based on the idea of disposable messages. Disposable letters can't be read after a max read limit is reached. A disposable chat room can't be joined after the max join limit is reached. The entrance to a persistent chat room can't be accessed after the max access limit is reached. This "access-based" approach gives a stronger guarantee of privacy than the "time-based" approach. If a message or chat room is set to be deleted after 15 minutes, nothing stops it from being viewed by unwanted parties at the 14th minute.

# Persistent Chat: Hideout's own invention!
Persistent chat rooms is a simple yet powerful idea. It's a private chat room that doesn't need to expire. By bookmarking it in the browser (or more securely, saving the link in a password manager), the participants can keep going back to the room, without having to create a new room every time they talk. Hideout achieves this in a very simple way.

Imagine you have already created a disposable chat room and you are chatting with your friends. Everything you say in this room is private. If you go ahead and create a second chat room, you can share the link of this new room privately with your friends. Since only you and your friends know about this new room, it doesn't have to expire. Hideout just automates this process.

Here are the details of how it's automated. A person creates a persistent chat room, and sets the number of participants to 4, for example. This makes Hideout generate a chat room that can be joined infinitely on the server. Then, Hideout generates an "entrance", which contains the room ID. The entrance can only be accessed 4 times. The person shares the link to this entrance (not the chat!) to their 3 friends. The 4 of them each opens the entrance, and joins the room from there. Hideout deletes the entrance after all 4 people have accessed it. So nothing else can get the room ID. But the 4 people now have a persistent chat room that they can always go back into.

Besides convenience, persistent chat is of great value for people who are in situations where it's improper to repeatedly create and share disposable chat room links, as doing so draws unwanted attention. In some countries, you can create and send 1000 room links on Discord every day, without the authority paying you a visit. But that's not the same in a lot of other places on the planet.

# Troubleshooting
If you received a link to a Hideout letter or chat, but it tells you that the maximum number of time it can be accessed is reached, then maybe some of the intended participants reloaded the letter page or rejoined the chat. If it's made certain that nobody is accessing multiple times, then the grim reality is probably that the communication among your friends is being spied on, and you all should move to secure messaging apps*.
* Actually, it doesn't necessarily mean that it's the unprivate apps like Facebook Messenger and Discord who's the spying adversary. Unprivate operating systems like Windows 10 can be the spying one too. Another possibility is that someone's computer is hacked, and moving to a secure messaging app only obscures the hack. Overall, one should follow good security and privacy practices.

# Why use Hideout, when there are already so many secure messaging apps?
The project of Hideout arose from personal needs.

I've been recommending my family and friends to use secure messaging apps like Signal, Wire, Element and so on. It wasn't easy. But some of them did sign up, installed the software, and started using it. But after a while, I noticed that the graph of my contacts on these apps form a star shape, where I'm the center. I have a lot of contacts on my end. But my friends don't have the initiative to further recommend those apps to their friends. I'm the only contact on their app for each of my friend.

It's getting extremely fast and simple to install a new app, sign up, and start chatting nowadays. But it's still too much efforts for some. It probably won't last too long for my friends, who installed an app and has only me on the contact list. And for the other friends who aren't using secure messaging apps, we are still talking about personal stuff on platforms that don't repsect user privacy.

That's when I started to work on Hideout. A chat service that requires no sign up, and no installation. You can create a chat room and send it to your contacts on unprivate platforms. But the conversation will remain private.

# Feature-richness
Hideout plans to be as feature-rich as possible. Emojis is already here. It just needs some polishing for ease of use. File-sharing and voice/vidoe chat are on the roadmap.


# Hideout is designed to be self-hosted.
Hideout is designed to be self-hosted. The idea is that the more privacy-minded and tech-savvy person among a friend group can set up a Hideout server, for them and their friends to use. The server is naturally trusted, as the owner is a friend.

However, "self-hosting" has a more strict meaning here. Ideally, the server should be run on a device that the person <ins>physically owns</ins>. To protect the server operator's IP address, the server should be run behind a VPN. I've experimented and confirmed that it's very practical. [Mullvad VPN](https://mullvad.net) has an open-source VPN client, and offers the ability of port-forwarding. The Hideout server I ran ended up having a URL followed by a port number, like https://<span></span>ww<span></span>w.myhideout.com:12345. Not a big deal since the URL can be bookmarked in the browser. Currently the only inconvenience is that Mullvad's port-forwarding is under development. I couldn't get more than 1 port forwarded. So I couldn't really SSH into my server at the same time.

A less ideal option is to rent a server from a VPS provider that's reputable for respecting user's privacy. Currently I can only think of [Njalla](https://njal.la/). But I'm open to suggestions on that.

The least ideal, almost unacceptable option is to rent a server from a VPS provider that doesn't necessarily respect user's privacy, like Google Cloud, Amazon AWS, and so on. If you can rent a server there, I don't see why you can't rent a server from Njalla.
Hideout isn't designed to be hosted on a server that gives service to a large population. People shouldn't place much trust on servers run by strangers. Futhermore, such a server will probably be spammed a lot, as Hideout does not have account registration.

# Functional Programming
Hideout is built with 2 functional programming languages: Haskell and Elm. Functional programming is a programming paradigm that deserves a lot more attention and should be the future. In brief, it encourages separation of side-effects, and type safety, which eliminates classes of bugs, while making refactoring and testing a charm, instead of a pain. It's also the first paradigm where I find myself actively seeking to learn more, because of its deep yet playful nature. The ultimate goal of functional programming is "If it compiles, it works." So if you prefer to solve compile errors, rather than wrestle with runtime bugs, you'll like functional programming.

I found [Learn You a Haskell for Great Good](http://learnyouahaskell.com/chapters) to be a great tutorial to get into functional programming with Haskell. Too bad it's non-HTTPS, so it's insecure to buy a copy with credit card. You can still read it for free though. Alternatively, [Elm](https://guide.elm-lang.org/) is famous for being easy to learn.
