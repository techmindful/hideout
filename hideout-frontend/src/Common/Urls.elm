module Common.Urls exposing (..)

import Url.Builder exposing (..)


mkBackendUrl : List String -> List QueryParameter -> String
mkBackendUrl pathSegments queryParams =
    absolute ( "api" :: pathSegments ) queryParams


rootUrl = absolute [] []


aboutUrl = absolute [ "about" ] []


aboutSectionUrl : String -> String
aboutSectionUrl sectionStr =
    custom Relative [ aboutUrl ] [] ( Just sectionStr )


frontendReadLetterUrl = absolute [ "read-letter" ] []
backendReadLetterUrl  = mkBackendUrl [ "read-letter" ] []


frontendWriteLetterUrl = absolute [ "write-letter" ] []
backendWriteLetterUrl  = mkBackendUrl [ "write-letter" ] []


configChatUrl = absolute [ "config-chat" ] []


backendSpawnDispChatUrl  = mkBackendUrl [ "spawn-disposable-chat" ] []
backendSpawnPersistChatUrl = mkBackendUrl [ "spawn-persistent-chat" ] []


frontendChatUrl chatId = absolute [ "chat", chatId ] []
backendChatUrl chatId = mkBackendUrl [ "chat", chatId ] []


sendMessageUrl chatId = mkBackendUrl [ "send-message", chatId ] []
