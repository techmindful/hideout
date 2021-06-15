module Utils.Types exposing (..)


type Trio
    = Empty
    | Positive
    | Negative


type PosIntInput
    = Good Int
    | Bad  String

strToPosIntInput : String -> PosIntInput
strToPosIntInput str =
    case String.toInt str of
        Nothing -> Bad str
        Just n  ->
            if n >= 1 then
                Good n
            else
                Bad str
 

posIntInputToStr : PosIntInput -> String
posIntInputToStr input =
    case input of
        Good n  -> String.fromInt n
        Bad str -> str
