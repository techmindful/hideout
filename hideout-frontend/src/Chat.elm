module Chat exposing
    ( ChatId
    , ChatStatus
    , Message
    , MessageBody
    , mkJoinMsg
    , mkMessageMsg
    , msgView
    )

import Element
import Element exposing ( Element )
import Json.Encode as JEnc
import Tagged exposing ( Tagged, untag )
import Utils.Utils exposing ( plainPara )


type ChatIdTag = ChatIdTag
type alias ChatId = Tagged ChatIdTag String


type MessageBodyTag = MessageBodyTag
type alias MessageBody = Tagged MessageBodyTag String


type alias Message =
    { body : MessageBody }


type alias ChatStatus =
    { id : ChatId
    , input : MessageBody
    , msgs : List Message
    }


mkJoinMsg = mkChatMsg "join" ""


mkMessageMsg = mkChatMsg "message"


mkChatMsg : String -> String -> String
mkChatMsg msgType msgBody =
    JEnc.encode 0 <| JEnc.object
        [ ( "msgType", JEnc.string msgType )
        , ( "msgBody", JEnc.string msgBody )
        ]


msgView : Message -> Element m
msgView msg =
    plainPara <| untag msg.body

