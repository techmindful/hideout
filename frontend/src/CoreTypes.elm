module CoreTypes exposing (..)

import Browser
import Browser.Navigation as Nav
import Route exposing (..)
import Url exposing (Url)


type alias Model =
    { route : Route
    , navKey : Nav.Key
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | Nop
