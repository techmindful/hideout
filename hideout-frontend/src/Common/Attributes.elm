module Common.Attributes exposing (..)

import Element exposing (Element)
import Html.Events
import Json.Decode as JDec


onEnterKey : msg -> Element.Attribute msg
onEnterKey m =
    Element.htmlAttribute <| Html.Events.on "keydown" <| enterKeyDecoder m


enterKeyDecoder : msg -> JDec.Decoder msg
enterKeyDecoder m =
    Html.Events.keyCode
        |> JDec.andThen
            (\keyCode ->
                if keyCode == 13 then
                    JDec.succeed m

                else
                    JDec.fail "Not enter key"
            )
