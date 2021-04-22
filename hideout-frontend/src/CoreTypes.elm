module CoreTypes exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Chat
import Http
import Letter exposing (..)
import Route exposing (..)
import Url exposing (Url)
import UserStatus exposing (..)
import Utils.Types exposing ( PosIntInput )


type alias Model =
    { route : Route
    , viewport : Dom.Viewport
    , navKey : Nav.Key
    , isWsReady : Bool
    , userStatus : UserStatus
    , letterInput : String
    , letterMaxReadCountInput : PosIntInput
    , letterPersistInput : Bool
    , dispChatMaxJoinCountInput : PosIntInput
    , persistChatMaxJoinCountInput : PosIntInput
    , chatStatus : Chat.Status
    , newNameInput : String

    , isShiftHeld : Bool

    , tempResp : String
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | GotViewport Dom.Viewport
    | GotReadLetterResp ( Result Http.Error LetterMeta )
    | LetterInput String
    | LetterMaxReadCountInput String
    | LetterPersistInput Bool
    | LetterSend
    | GotLetterSendResp ( Result Http.Error String )

    | DispChatMaxJoinCountInput String
    | SpawnDispChat
    | GotSpawnDispChatResp ( Result Http.Error String )
 
    | PersistChatMaxJoinCountInput String
    | SpawnPersistChat
    | GotSpawnPersistChatResp ( Result Http.Error String )

    | MessageInput String
    | MessageSend
    | NewNameInput String
    | NameChange
    | OnWsReady String
    | OnWsMsg String
    | ChatMsgsViewEvent Chat.MsgsViewEvent
    | OnChatInputFocal Bool

    | OnWindowResized

    | OnKeyDown String
    | OnKeyUp   String

    | Nop

