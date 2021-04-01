module Letter exposing (..)

import Json.Decode as JDec
import Json.Decode exposing ( field, string )

type alias Letter =
    { body : String }


jsonDec : JDec.Decoder Letter
jsonDec = JDec.map Letter <| field "body" string

