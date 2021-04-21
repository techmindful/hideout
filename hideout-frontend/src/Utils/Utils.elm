module Utils.Utils exposing (..)

import Browser.Dom as Dom


hasManualScrolledUp : Dom.Viewport -> Float -> Bool
hasManualScrolledUp viewport margin =
    if viewport.viewport.y
     + viewport.viewport.height
     + margin
     < viewport.scene.height
    then True
    else False


