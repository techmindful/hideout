module Letter exposing (..)

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


letterJsonDec : JDec.Decoder Letter
letterJsonDec = JDec.map2 Letter ( field "letterBody" string ) ( field "letterMaxReadCount" int )


letterMetaJsonDec : JDec.Decoder LetterMeta
letterMetaJsonDec =
    JDec.map2 LetterMeta
        ( field "letterMetaLetter" letterJsonDec )
        ( field "letterMetaReadCount" int )

