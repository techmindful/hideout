module CoreTypes exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Route exposing (..)
import Url exposing (Url)


type alias Model =
    { route : Route
    , viewport : Result Dom.Error Dom.Viewport
    , navKey : Nav.Key
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | GotViewport (Result Dom.Error Dom.Viewport)
    | Nop
