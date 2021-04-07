module Chat exposing
    ( ChatId
    , Message
    , MessageBody
    )

import Tagged exposing ( Tagged )


type Message = Message
    { body : MessageBody }


type ChatIdTag = ChatIdTag
type alias ChatId = Tagged ChatIdTag String


type MessageBodyTag = MessageBodyTag
type alias MessageBody = Tagged MessageBodyTag String

