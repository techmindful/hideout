module UserStatus exposing (..)

import Chat
import Letter exposing (..)
import Http


type UserStatus
    = ReadLetterReq String
    | ReadLetterResp ( Result Http.Error LetterMeta )
    | GotPersistChatIdLetter ( Result Http.Error String )
    | Other
