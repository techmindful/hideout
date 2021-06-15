module Letter exposing (..)

import Http
import Json.Decode as JDec
import Json.Decode exposing ( field, string, int )
import Utils.Types exposing
    ( PosIntInput(..)
    , Trio(..)
    , strToPosIntInput
    )


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
    | GotError Http.Error
    | GotResp
        { id : String
        , maxReadCount : Int
        , copyToClipboardResult : Trio
        }


type alias Letter =
    { body : String
    , maxReadCount : Int
    }
type alias LetterMeta =
    { letter : Letter
    , readCount  : Int
    }
letterJsonDec : JDec.Decoder Letter
letterJsonDec = JDec.map2 Letter ( field "body" string ) ( field "maxReadCount" int )
letterMetaJsonDec : JDec.Decoder LetterMeta
letterMetaJsonDec =
    JDec.map2 LetterMeta
        ( field "letter" letterJsonDec )
        ( field "readCount" int )


type InputError
    = EmptyBody
    | BadMaxReadCount
type alias RawInput =
    { body : String
    , maxReadCount : String
    }
type alias GoodInput =
    { body : String
    , maxReadCount : Int
    }
validateInput : RawInput -> Result InputError GoodInput
validateInput input =
    input
        |> ( \input_ -> if String.isEmpty input_.body then Err EmptyBody
                        else Ok input_
           )
        |> Result.andThen
           ( \input_ ->
               case strToPosIntInput input_.maxReadCount of
                   Bad _  -> Err BadMaxReadCount
                   Good posInt -> Ok { body = input_.body, maxReadCount = posInt }
           )

