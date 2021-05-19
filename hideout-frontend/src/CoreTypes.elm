module CoreTypes exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Chat
import Http
import Json.Decode as JDec
import Letter exposing ( LetterMeta )
import Route exposing (..)
import Url exposing (Url)
import Utils.Types exposing ( PosIntInput )
import Views.About


type State
    = ErrGetHost
    | Normal Model


type alias InitFlag =
    { protocol : String
    , host : String
    }
initFlagDecoder : JDec.Decoder InitFlag
initFlagDecoder =
    JDec.map2
        InitFlag
            ( JDec.field "protocol" JDec.string )
            ( JDec.field "host" JDec.string )


type WsStatus
    = NotOpened
    | Opened
    | Error


type SpawnPersistChatResp
    = NotSpawned
    | Waiting
    | GotError Http.Error
    | GotLetterId String


type alias Model =
    { protocol : String
    , host : String
    , origin : String
    , route : Route

    , viewport : Dom.Viewport
    , navKey : Nav.Key
    , wsStatus : WsStatus
    , windowVisibility : Browser.Events.Visibility

    , aboutPageModel : Views.About.Model

    , joinChatInput : String

    , spawnPersistChatResp : SpawnPersistChatResp

    , letterRawInput : Letter.RawInput
    , letterPersistInput : Bool
    , letterStatus : Letter.Status
    , dispChatMaxJoinCountInput : String
    , persistChatMaxJoinCountInput : String
    , chatStatus : Chat.Status
    , newNameInput : String

    , isShiftHeld : Bool

    , tempResp : String
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | GotViewport Dom.Viewport

    | AboutPageMsg Views.About.Msg

    | JoinChatInput String
    | JoinChat

    -- Letter
    | GotReadLetterResp ( Result Http.Error LetterMeta )
    | LetterInput String
    | LetterMaxReadCountInput String
    | LetterPersistInput Bool
    | LetterSend
    | GotLetterSendResp ( Result Http.Error String )

    -- Config disp chat
    | DispChatMaxJoinCountInput String
    | SpawnDispChat
    | GotSpawnDispChatResp ( Result Http.Error String )
 
    -- Config persist chat
    | PersistChatMaxJoinCountInput String
    | SpawnPersistChat
    | GotSpawnPersistChatResp ( Result Http.Error String )

    -- Chatting
    | MessageInput String
    | MessageSend
    | NewNameInput String
    | NameChange
    | OnWsReady String
    | OnWsError
    | OnWsMsg String
    | ChatMsgsViewEvent Chat.MsgsViewEvent
    | OnChatInputFocal Bool

    | OnWindowResized

    | OnVisibilityChange Browser.Events.Visibility

    | OnKeyDown String
    | OnKeyUp   String

    | Nop

