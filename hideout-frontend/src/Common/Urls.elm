module Common.Urls exposing (..)

import Url.Builder exposing (..)


frontendUrl = "http://localhost:8000"
backendUrl = "http://localhost:8080"


mkBackendUrl = crossOrigin backendUrl


rootUrl = absolute [] []


aboutUrl = absolute [ "about" ] []


frontendReadLetterUrl = crossOrigin frontendUrl [ "read-letter" ] []
backendReadLetterUrl  = mkBackendUrl [ "read-letter" ] []


frontendWriteLetterUrl = absolute [ "write-letter" ] []
backendWriteLetterUrl  = mkBackendUrl [ "write-letter" ] []


configChatUrl = absolute [ "config-chat" ] []


backendSpawnDispChatUrl  = mkBackendUrl [ "spawn-disposable-chat" ] []
backendSpawnPersistChatUrl = mkBackendUrl [ "spawn-persistent-chat" ] []


chatUrl chatId = relative [ "chat", chatId ] []


sendMessageUrl chatId = crossOrigin backendUrl [ "send-message", chatId ] []
