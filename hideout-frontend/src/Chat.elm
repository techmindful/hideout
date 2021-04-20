module Chat exposing
    ( ChatId
    , Status
    , MsgFromServer
    , MsgBody
    , MsgBundle
    , mkJoinMsg
    , mkContentMsg
    , mkMsgBundles
    , mkNameChangeMsg
    , msgFromServerDecoder
    , WsMsg(..)
    , wsMsgDecoder
    )

import Common.Contents exposing ( plainPara )
import Element
import Element exposing ( Element )
import Json.Decode as JDec
import Json.Encode as JEnc
import List.Extra as List
import Tagged exposing ( Tagged, tag, untag )


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


{-| A list of messages that a user has sent continuously, to be displayed together, without adding a username header at each message.
-}
type alias MsgBundle =
    { username : String
    , msgs : List MsgFromServer
    }
mkMsgBundles : List MsgFromServer -> List MsgBundle
mkMsgBundles msgFromServers =
    let
        combine : MsgFromServer -> List MsgBundle -> List MsgBundle
        combine msg bundles =
            case
                Maybe.map2 Tuple.pair
                    ( List.head bundles )
                    ( List.tail bundles ) of

                Just ( headBundle, tailBundles ) ->
                    if msg.username == headBundle.username then
                        let
                            updatedHeadBundle = 
                                { headBundle | msgs = msg :: headBundle.msgs }
                        in
                        updatedHeadBundle :: tailBundles
                    else
                        { username = msg.username, msgs = [ msg ] } :: bundles

                Nothing ->
                    [ { username = msg.username, msgs = [ msg ] } ]

    in
    List.foldr combine [] msgFromServers


