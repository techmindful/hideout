module Common.Urls exposing (..)

import Url.Builder exposing (..)


frontendUrl = "http://localhost:8000"


backendUrl = "http://localhost:8080"


frontendReadLetterUrl = crossOrigin frontendUrl [ "read-letter" ] []


backendReadLetterUrl = crossOrigin backendUrl [ "read-letter" ] []


backendWriteLetterUrl = crossOrigin backendUrl [ "write-letter" ] []
