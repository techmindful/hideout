module Chat exposing
    ( ChatId
    , ChatStatus
    , MessageBody
    , mkJoinMsg
    , mkMessageMsg
    )

import Json.Encode as JEnc
import Tagged exposing ( Tagged )


type ChatIdTag = ChatIdTag
type alias ChatId = Tagged ChatIdTag String


type MessageBodyTag = MessageBodyTag
type alias MessageBody = Tagged MessageBodyTag String


type alias ChatStatus =
    { id : ChatId
    , input : MessageBody
    }


mkJoinMsg = mkChatMsg "join" ""


mkMessageMsg = mkChatMsg "message"


mkChatMsg : String -> String -> String
mkChatMsg msgType msgBody =
    JEnc.encode 0 <| JEnc.object
        [ ( "msgType", JEnc.string msgType )
        , ( "msgBody", JEnc.string msgBody )
        ]

