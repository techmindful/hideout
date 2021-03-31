module UserStatus exposing (..)


type UserStatus
    = WritingLetter
    | SentLetter
    | GotLetterId String
    | Other
