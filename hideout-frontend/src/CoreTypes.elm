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
import Time
import Url exposing (Url)
import Utils.Types exposing ( PosIntInput )
import Views.About
import Views.Entrance


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


type SpawnDispChatResp
    = NotSpawned_Disp
    | Waiting_Disp
    | GotError_Disp Http.Error
    | GotChatId String


type SpawnPersistChatResp
    = NotSpawned_Persist
    | Waiting_Persist
    | GotError_Persist Http.Error
    | GotEntranceId String


type alias Model =
    { protocol : String
    , host : String
    , origin : String
    , route : Route

    , viewport : Dom.Viewport
    , navKey : Nav.Key
    , windowVisibility : Browser.Events.Visibility

    , aboutPageModel : Views.About.Model

    , joinChatInput : String

    , spawnDispChatResp : SpawnDispChatResp
    , spawnPersistChatResp : SpawnPersistChatResp

    , entranceStatus : Views.Entrance.Status

    , letterRawInput : Letter.RawInput
    , letterPersistInput : Bool
    , letterStatus : Letter.Status
    , dispChatMaxJoinCountInput : String
    , persistChatMaxJoinCountInput : String
    , chatStatus : Chat.Status

    , isShiftHeld : Bool

    , time : Time.Posix

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
    | OnShareEntrance String

    | GotEntranceResp ( Result Http.Error String )

    | ChatElmMsg Chat.ElmMsg

    | OnWindowResized

    | OnVisibilityChange Browser.Events.Visibility

    | OnKeyDown String
    | OnKeyUp   String

    | GotTime Time.Posix

    | Nop

