module Utils.Utils exposing (..)

import Browser.Dom as Dom
import Time exposing
    ( Month(..)
    , Posix
    , millisToPosix
    , toYear
    , toMonth
    , toDay
    , toHour
    , toMinute
    , toSecond
    , utc
    )


hasManualScrolledUp : Dom.Viewport -> Float -> Bool
hasManualScrolledUp viewport margin =
    if viewport.viewport.y
     + viewport.viewport.height
     + margin
     < viewport.scene.height
    then True
    else False


formatTime : Int -> Posix -> String
formatTime targetPosixSec currentPosix =
    let
        targetPosixMilliSec = targetPosixSec * 1000

        targetPosix = millisToPosix targetPosixMilliSec

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



