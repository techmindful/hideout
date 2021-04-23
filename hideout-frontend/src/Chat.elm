module Chat exposing
    ( ChatId
    , ChatMsgMeta
    , CtrlMsg(..)
    , Err(..)
    , MsgBody
    , MsgBundle
    , MsgType(..)
    , MsgsViewEvent(..)
    , Status
    , autoScrollMargin
    , isMetaBundle
    , mkJoinMsg
    , mkContentMsg
    , mkMsgBundles
    , mkNameChangeMsg
    , chatMsgMetaDecoder
    , WsMsg(..)
    , wsMsgDecoder
    )

import Browser.Dom as Dom
import Common.Contents exposing ( plainPara )
import Dict exposing ( Dict )
import Element
import Element exposing ( Element )
import Json.Decode as JDec
import Json.Decode.Extra as JDec
import Json.Encode as JEnc
import List.Extra as List
import Tagged exposing ( Tagged, tag, untag )
import Time exposing ( Posix )
import Utils.Utils as Utils exposing ( is )


type ChatIdTag = ChatIdTag
type alias ChatId = Tagged ChatIdTag String


type MsgType
    = Content
    | Join
    | NameChange
    | Leave


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
        ( JDec.field "msgType" JDec.string
            |> JDec.andThen
                ( \str ->
                    if str == "content" then JDec.succeed Content
                    else if str == "join" then JDec.succeed Join
                    else if str == "nameChange" then JDec.succeed NameChange
                    else if str == "leave" then JDec.succeed Leave
                    else JDec.fail "Invalid MsgType"
                )
        )
        ( JDec.map tag <| JDec.field "msgBody" JDec.string )


type CtrlMsg
    = Err_ Err

type Err
    = MaxJoined
    | NotFound

ctrlMsgTypeDecoder : JDec.Decoder String
ctrlMsgTypeDecoder = JDec.field "msgType" JDec.string

type CtrlMsgBodyTag = CtrlMsgBodyTag
type alias CtrlMsgBody = Tagged CtrlMsgBodyTag String

errCtrlMsgDecoder : JDec.Decoder CtrlMsg
errCtrlMsgDecoder =
    JDec.map Err_
        ( JDec.field "msgBody" JDec.string
            |> JDec.andThen
                ( \str ->
                    if str == "maxJoined" then JDec.succeed MaxJoined
                    else if str == "notFound" then JDec.succeed NotFound
                    else JDec.fail "Invalid error ctrl msg type"
                )
        )

ctrlMsgDecoder : JDec.Decoder CtrlMsg
ctrlMsgDecoder =
    JDec.oneOf
        [ JDec.when ctrlMsgTypeDecoder ( is "err" ) errCtrlMsgDecoder ]


type alias ChatMsgMeta =
    { msgFromClient : MsgFromClient
    , userId : Int

    -- Without this field, username in previous messages goes unfound when user leaves.
    , username : String  

    , posixTimeSec : Int
    }
chatMsgMetaDecoder : JDec.Decoder ChatMsgMeta
chatMsgMetaDecoder =
      JDec.map4
        ChatMsgMeta
        ( JDec.field "msgFromClient" msgFromClientDecoder )
        ( JDec.field "userId" JDec.int )
        ( JDec.field "username" JDec.string )
        ( JDec.field "posixTimeSec" JDec.int )


type alias MsgHistory =
    { msgs  : List ChatMsgMeta 
    , users : Dict Int String
    , maxJoinCount : Maybe Int
    }
msgHistoryDecoder : JDec.Decoder MsgHistory
msgHistoryDecoder =
    JDec.map3
        MsgHistory
        ( JDec.field "msgs"  <| JDec.list chatMsgMetaDecoder )
        ( JDec.field "users" <| JDec.dict2 JDec.int JDec.string )
        ( JDec.maybe <| JDec.field "maxJoinCount" JDec.int )


type alias UserIdMsg = { yourUserId : Int }
userIdMsgDecoder : JDec.Decoder UserIdMsg
userIdMsgDecoder = JDec.map UserIdMsg <| JDec.field "yourUserId" JDec.int


-- A type for conveniently JSON-decoding an incoming ws string
-- Into various possible Elm types.
type WsMsg
    = ChatMsgMeta_ ChatMsgMeta
    | CtrlMsg_ CtrlMsg
    | MsgHistory_ MsgHistory
    | UserIdMsg_ UserIdMsg
wsMsgDecoder : JDec.Decoder WsMsg
wsMsgDecoder =
    JDec.oneOf
        [ JDec.map ChatMsgMeta_ chatMsgMetaDecoder
        , JDec.map CtrlMsg_ ctrlMsgDecoder
        , JDec.map MsgHistory_ msgHistoryDecoder
        , JDec.map UserIdMsg_ userIdMsgDecoder
        ]


type alias Status =
    { chatId : ChatId
    , myUserId : Int
    , input : MsgBody
    , msgs : List ChatMsgMeta
    , users : Dict Int String

    , maxJoinCount : Maybe Int
    , joinCount : Int

    , hasManualScrolledUp : Bool
    , shouldHintNewMsg : Bool

    , isInputFocused : Bool

    , err : Maybe Err
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
    { userId : Int  -- Compare with this, not username.
    , username : String
    , msgs : List ChatMsgMeta
    , time : Posix
    }


mkMsgBundles : List ChatMsgMeta -> List MsgBundle
mkMsgBundles chatMsgMetas =
    let
        combine : ChatMsgMeta -> List MsgBundle -> List MsgBundle
        combine msg bundles =
            let
                appended =
                    { userId = msg.userId
                    , username = msg.username
                    , msgs = [ msg ]
                    , time = Utils.posixSecToPosix msg.posixTimeSec
                    } :: bundles
            in
            case
                Maybe.map2 Tuple.pair
                    ( List.head bundles )
                    ( List.tail bundles ) of

                Just ( headBundle, tailBundles ) ->
                    if isMetaMsg msg ||
                       ( Maybe.withDefault False <| Maybe.map isMetaMsg <| List.head headBundle.msgs )
                    then appended
                    else
                        if msg.userId == headBundle.userId then
                            let
                                updatedHeadBundle = 
                                    { headBundle | msgs = msg :: headBundle.msgs }
                            in
                            updatedHeadBundle :: tailBundles
                        else
                            appended

                -- Empty bundle list
                Nothing ->
                    appended

    in
    List.foldr combine [] chatMsgMetas


isMetaBundle : MsgBundle -> Bool
isMetaBundle bundle =
    case Maybe.map isMetaMsg <| List.head bundle.msgs of
        Just True -> True
        _ -> False


isMetaMsg : ChatMsgMeta -> Bool
isMetaMsg msg =
    case msg.msgFromClient.msgType of
        Content -> False
        _ -> True


type MsgsViewEvent
    = TriedSnapScroll ( Result Dom.Error () )
    | OnManualScrolled
    | GotViewport ( Result Dom.Error Dom.Viewport )
    | OnNewMsgHintClicked


{-| If user hasn't manually scrolled up more than this value,
then allow auto scrolling when new chat messages come in.

Tested with Main's logViewport. One scroll wheel was 53.

This is also good for floating point comparison.
-}
autoScrollMargin : Float
autoScrollMargin = 30

