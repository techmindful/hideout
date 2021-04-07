module Chat exposing (..)

import Tagged exposing ( Tagged )


type Message = Message
    { body : String }


type ChatIdTag = ChatIdTag


type alias ChatId = Tagged ChatIdTag String

