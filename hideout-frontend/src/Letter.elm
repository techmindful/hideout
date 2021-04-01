module Letter exposing (..)

import Json.Decode as JDec
import Json.Decode exposing ( field, string, int )

type alias Letter =
    { body : String
    , maxReads : Int
    }


type alias LetterMeta =
    { letter : Letter
    , reads  : Int
    }


letterJsonDec : JDec.Decoder Letter
letterJsonDec = JDec.map2 Letter ( field "body" string ) ( field "maxReads" int )


letterMetaJsonDec : JDec.Decoder LetterMeta
letterMetaJsonDec = JDec.map2 LetterMeta ( field "letter" letterJsonDec ) ( field "reads" int )

