module Letter exposing (..)

import Http
import Json.Decode as JDec
import Json.Decode exposing ( field, string, int )

type alias Letter =
    { body : String
    , maxReadCount : Int
    }


type alias LetterMeta =
    { letter : Letter
    , readCount  : Int
    }


type alias Status =
    { read : ReadStatus
    , write : WriteStatus
    }


type ReadStatus
    = Init
    | Waiting
    | Got ( Result Http.Error LetterMeta )


type WriteStatus
    = NotSent
    | Sent
        { maxReadCount : Int }
    | GotResp ( Result Http.Error
        { id : String
        , maxReadCount : Int
        }
      )


letterJsonDec : JDec.Decoder Letter
letterJsonDec = JDec.map2 Letter ( field "body" string ) ( field "maxReadCount" int )


letterMetaJsonDec : JDec.Decoder LetterMeta
letterMetaJsonDec =
    JDec.map2 LetterMeta
        ( field "letter" letterJsonDec )
        ( field "readCount" int )

