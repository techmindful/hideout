To build Hideout, you need:
* Elm: https://elm-lang.org/
* Haskell Stack: https://docs.haskellstack.org/en/stable/README/

Clone the repo. This guide assumes it'll be at `/home/user/Projects/hideout/`.

Enter `hideout-frontend/` and do:
```
elm make src/Main.elm --optimize --output=main.js
```
This compiles Elm code into `main.js`.

Enter `hideout-backend/` and do:
```
stack install
```
This compiles Haskell code into an executable file at `~/.local/bin/hideout-backend-exe`.

The pieces to run Hideout are
* `hideout-frontend/index.html`
* `hideout-frontend/main.js`
* `hideout-frontend/static/`
* `~/.local/bin/hideout-backend-exe`

To host Hideout, see [hosting.md](https://github.com/techmindful/hideout/blob/main/hosting.md)

To develop Hideout, you further need:
* nginx: https://nginx.org/en/docs/install.html, for a working local server.
* elm-live: https://github.com/wking-io/elm-live, for hot-reloading.

I usually enter `hideout-frontend/` and do:
```
elm-live src/Main.elm --pushstate -- --output=main.js
```
And keep a browser tab open at `localhost:8000`. This allows me to see compiler errors on a full screen, rather than a small tmux pane.

But `elm-live` can't get Hideout working locally. For that, we need nginx.

Nginx requires that server files are all "accessible". Specifically, try to `stat` into Hideout's directory, like:
```
stat ~/Projects/hideout/hideout-frontend/main.js
```
If it gives you some statistics, then it's all set. If it says "permission denied", then you need to use `chmod` to give `x` permission to all the directories leading to Hideout. In my case, I needed to do `chmod +x ~`. This is from a StackOverflow answer here: https://stackoverflow.com/a/43686446

If your repo is at a different location than `/home/user/Projects/hideout`, then you need to modify the `root` directive at [this line](https://github.com/techmindful/hideout/blob/cb206c46f7e3191fec2d37ca05c4aa93534769f9/nginx.conf#L7) in your `nginx.conf`, to point to your location of `hideout-frontend/`.

Under `hideout/`, do:
```
sudo nginx -c $(pwd)/nginx.conf
```
This starts an nginx server, using a config provided by the repo.

Finally, run `~/.local/bin/hideout-backend-exe`. If `~/.local/bin` is on your `$PATH`, you can just do:
```
hideout-backend-exe
```

The server is now running on `localhost`.
