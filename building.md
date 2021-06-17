To build Hideout, you need:
* Elm: https://elm-lang.org/
* Haskell Stack: https://docs.haskellstack.org/en/stable/README/

After cloning the repo, enter `hideout-frontend/` and do:
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

But `elm-live` can't get Hideout working locally. For that, we need nginx. Under `hideout/`, do:
```
sudo nginx -c $(pwd)/nginx.conf
```
This starts an nginx server, using a config provided by the repo.
