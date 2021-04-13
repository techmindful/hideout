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


type MaxReadCountInput
    = Good Int
    | Bad  String


maxReadCountInputToStr : MaxReadCountInput -> String
maxReadCountInputToStr input =
    case input of
        Good n  -> String.fromInt n
        Bad str -> str


letterJsonDec : JDec.Decoder Letter
letterJsonDec = JDec.map2 Letter ( field "body" string ) ( field "maxReadCount" int )


letterMetaJsonDec : JDec.Decoder LetterMeta
letterMetaJsonDec = JDec.map2 LetterMeta ( field "letter" letterJsonDec ) ( field "readCount" int )

