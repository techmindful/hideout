module Utils.Utils exposing (..)

import Browser.Dom as Dom
import Time exposing
    ( Month(..)
    , Posix
    , millisToPosix
    , posixToMillis
    , toYear
    , toMonth
    , toDay
    , toHour
    , toMinute
    , toSecond
    , utc
    )


is : a -> a -> Bool
is x y = x == y


hasManualScrolledUp : Dom.Viewport -> Float -> Bool
hasManualScrolledUp viewport margin =
    if viewport.viewport.y
     + viewport.viewport.height
     + margin
     < viewport.scene.height
    then True
    else False


{-| Limit a string to n characters. Replace the rest with "..." -}
capString : Int -> String -> String
capString n str =
    if String.length str <= n then
        str
    else
        String.left n str ++ "..."


formatTime : Posix -> Posix -> String
formatTime targetPosix currentPosix =
    let
        year  = toYear   utc targetPosix 
        month = toMonth  utc targetPosix |> monthToInt
        day   = toDay    utc targetPosix 
        hour  = toHour   utc targetPosix 
        min   = toMinute utc targetPosix 
        sec   = toSecond utc targetPosix 

        date = String.join "/" <| List.map String.fromInt [ year, month, day ]
        time = String.join ":" <| List.map String.fromInt [ hour, min, sec ]

        whole = String.join " - " [ date, time ]
    in
    whole


intToOrdStr : Int -> String
intToOrdStr n =
    let
        postfix =
            if modBy 100 n >= 10 && modBy 100 n <= 20 then
                "th"
            else
                if modBy 10 n == 1 then "st"
                else if modBy 10 n == 2 then "nd"
                else if modBy 10 n == 3 then "rd"
                else "th"
    in
    ( String.fromInt n ) ++ postfix


posixSecToPosix : Int -> Posix
posixSecToPosix posixSec =
    let
        posixMilliSec = posixSec * 1000
    in
    millisToPosix posixMilliSec


durationSec : Time.Posix -> Time.Posix -> Int
durationSec startTime endTime =
    round ( toFloat ( posixToMillis endTime - posixToMillis startTime ) / 1000 )


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12

