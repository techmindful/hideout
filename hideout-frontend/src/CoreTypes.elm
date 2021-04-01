module CoreTypes exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Http
import Letter exposing ( Letter )
import Route exposing (..)
import Url exposing (Url)
import UserStatus exposing (..)


type alias Model =
    { route : Route
    , viewport : Result Dom.Error Dom.Viewport
    , navKey : Nav.Key
    , userStatus : UserStatus
    , letterInput : String
    , tempResp : String
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | GotViewport ( Result Dom.Error Dom.Viewport )
    | GotReadLetterResp ( Result Http.Error Letter )
    | LetterInput String
    | LetterSend
    | GotLetterSendResp ( Result Http.Error String )
    | Nop
