module Chat exposing
    ( ChatId
    , Status
    , Msg
    , MsgBody
    , mkJoinMsg
    , mkContentMsg
    , msgDecoder
    )

import Element
import Element exposing ( Element )
import Json.Decode as JDec
import Json.Encode as JEnc
import Tagged exposing ( Tagged, tag, untag )
import Utils.Utils exposing ( plainPara )


type ChatIdTag = ChatIdTag
type alias ChatId = Tagged ChatIdTag String


type MsgTypeTag = MsgTypeTag
type alias MsgType = Tagged MsgTypeTag String


type MsgBodyTag = MsgBodyTag
type alias MsgBody = Tagged MsgBodyTag String


type alias Msg =
    { msgType : MsgType
    , msgBody : MsgBody
    }


msgDecoder : JDec.Decoder Msg
msgDecoder =
    JDec.map2
        Msg
        ( JDec.map tag <| JDec.field "msgType" JDec.string )
        ( JDec.map tag <| JDec.field "msgBody" JDec.string )


type alias Status =
    { id : ChatId
    , input : MsgBody
    , msgs : List Msg
    }


mkJoinMsg = mkWsMsg "join" ""


mkContentMsg = mkWsMsg "content"


mkWsMsg : String -> String -> String
mkWsMsg msgType msgBody =
    JEnc.encode 0 <| JEnc.object
        [ ( "msgType", JEnc.string msgType )
        , ( "msgBody", JEnc.string msgBody )
        ]

