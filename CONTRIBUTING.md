# Welcome

Hideout welcomes your contribution! It's a project of moderate complexity, and has the ambition of competing with popular chat services. It's unlikely I can give it the robustness it deserves on my own. In order for us to work together better, let's look at some guidelines on communication and contribution.

# Communication

The communication rule I'd like to follow is that, if a contribution you want to make will take a non-trivial amount of effort, talk to me first. Read further on contributing, to see how this rule applies.

# Contributing

Here are the ways Hideout needs your contribution. If a way of contribution isn't listed, first assume it's not encouraged, then open an issue and ask me about it.

### Trying to break it

It really is just a more exciting way of saying "testing" 😆 But right now testing is what Hideout needs the most. I couldn't cover that much of testing when I'm buliding a chat service by myself. Do messages go missing or out of order? Can Hideout handle 10000 messages in a room? How about 1000 rooms on a server? Does the server crash? Can it be hacked? What will happen if you fuzz it...

Every bug you find is of immense value! Just make sure you've checked if it's already been reported before, by searching through existing issues. If not, open a new one. Describe how to reproduce it in details. Potential cause of the bug, and potential ways to fix it are great bonuses.

### Automated Tests

It really is no fun checking if the basic messaging works, every time the code is rebuilt. I'm not very experienced with automated testing yet. So any help on that part will save me a lot of sanity 🙏 But before you write a new automated test, talk to me first by opening an issue. What's the issue here? You think there lacks a certain automated test! Since writing a test will take some time, let's discuss whether it's needed, how it should be done, who should write it, before committing to the effort.

### Feature Improvement

Let's have every feature-improving code start with an issue. For example, you may want to write some code to have emoji picker's scrollbar retain its position, when the picker is re-opened. That is because the emoji picker has an issue: The scrollbar resets to top every time the picker is opened! So open an issue first, and then we can talk about how to improve it, and who should improve it.

### Cosmetic Changes?

Cosmetic changes are changes that improve code readability, but don't impact any functionality. For now, typo fixes are welcomed. Reformatting of the same piece of code is discouraged. Substitution with cleaner expressions may or may not be merged.

### Major Feature Implementation 🚫

Currently, I decide to leave major feature implementation like voice/video chat, file sharing, end-to-end encryption, or even slightly smaller ones like emoji categorization, to myself. A task like those will take a lot of efforts. Even if communication and planning are established beforehand, it's still very possible that the result implementation doesn't play well with the code base, and can't be merged.

However, I will open an issue myself, for each such feature, to start a discussion on what's the proper way to implement it. Everyone is welcomed in the discussion.

So yeah, I'm saving the fun parts for myself, and leaving the boring stuff for others 😨 But the reality is, nobody can take the fun away. You can always fork the repo and play with it however you want! It's just that, you may start out not caring about your fork, but in case you end up implementing an entire shiny feature like end-to-end encrypted voice/video chat, you may really want it merged. Be very careful of the expectation of having your code merged creeping up on you.

In summary, don't let me stop you from having fun with implementing new features on your fork. Just know that if you did it, it may still not be merged. On the plus side, you had fun doing it and learned a lot! Even better, you can provide everyone with advice and caution about implementing that feature. These will surely be merged, with the mind 💡
