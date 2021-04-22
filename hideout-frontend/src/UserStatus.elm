module UserStatus exposing (..)

import Chat
import Letter exposing (..)
import Http


type UserStatus
    = GotPersistChatIdLetter ( Result Http.Error String )
    | Other
