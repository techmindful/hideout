module UserStatus exposing (..)

import Letter exposing (..)

import Http


type UserStatus
    = ReadLetterReq String
    | ReadLetterResp ( Result Http.Error LetterMeta )
    | WritingLetter
    | SentLetter
    | GotLetterId String
    | Other
