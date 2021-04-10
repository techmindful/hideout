module UserStatus exposing (..)

import Chat exposing ( ChatId, MessageBody )
import Letter exposing (..)
import Http


type UserStatus
    = ReadLetterReq String
    | ReadLetterResp ( Result Http.Error LetterMeta )
    | WritingLetter
    | SentLetter
    | GotLetterId String  -- Got letter ID from server after sending the letter.
    | Other