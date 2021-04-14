module Chat exposing
    ( ChatId
    , Status
    , MsgFromServer
    , MsgBody
    , mkJoinMsg
    , mkContentMsg
    , mkNameChangeMsg
    , msgFromServerDecoder
    , WsMsg(..)
    , wsMsgDecoder
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


type alias MsgFromClient =
    { msgType : MsgType
    , msgBody : MsgBody
    }
msgFromClientDecoder : JDec.Decoder MsgFromClient
msgFromClientDecoder =
    JDec.map2
        MsgFromClient
        ( JDec.map tag <| JDec.field "msgType" JDec.string )
        ( JDec.map tag <| JDec.field "msgBody" JDec.string )


type alias MsgFromServer =
    { msgFromClient : MsgFromClient
    , username : String
    }
msgFromServerDecoder : JDec.Decoder MsgFromServer
msgFromServerDecoder =
    JDec.map2
        MsgFromServer
        ( JDec.field "msgFromClient" msgFromClientDecoder )
        ( JDec.field "username" JDec.string )


type alias MsgHistory =
    { msgs  : List MsgFromServer
    , users : List String
    }
msgHistoryDecoder : JDec.Decoder MsgHistory
msgHistoryDecoder =
    JDec.map2
        MsgHistory
        ( JDec.field "msgs"  <| JDec.list msgFromServerDecoder )
        ( JDec.field "users" <| JDec.list JDec.string )


-- A type for conveniently JSON-decoding an incoming ws string
-- Into various possible Elm types.
type WsMsg
    = MsgFromServer_ MsgFromServer
    | MsgHistory_ MsgHistory
wsMsgDecoder : JDec.Decoder WsMsg
wsMsgDecoder =
    JDec.oneOf
        [ JDec.map MsgFromServer_ msgFromServerDecoder
        , JDec.map MsgHistory_ msgHistoryDecoder
        ]


type alias Status =
    { id : ChatId
    , input : MsgBody
    , msgs : List MsgFromServer
    , users : List String
    }


mkJoinMsg : ChatId -> String
mkJoinMsg = mkWsMsg "join" << untag


mkContentMsg : MsgBody -> String
mkContentMsg = mkWsMsg "content" << untag


mkNameChangeMsg : MsgBody -> String
mkNameChangeMsg = mkWsMsg "nameChange" << untag


mkWsMsg : String -> String -> String
mkWsMsg msgType msgBody =
    JEnc.encode 0 <| JEnc.object
        [ ( "msgType", JEnc.string msgType )
        , ( "msgBody", JEnc.string msgBody )
        ]

