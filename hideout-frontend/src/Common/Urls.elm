module Common.Urls exposing (..)

import Url.Builder exposing (..)


mkBackendUrl : List String -> List QueryParameter -> String
mkBackendUrl pathSegments queryParams =
    absolute ("api" :: pathSegments) queryParams


rootUrl =
    absolute [] []


aboutUrl =
    absolute [ "about" ] []


frontendReadLetterUrl letterId =
    absolute [ "read-letter", letterId ] []


backendReadLetterUrl =
    mkBackendUrl [ "read-letter" ] []


frontendWriteLetterUrl =
    absolute [ "write-letter" ] []


backendWriteLetterUrl =
    mkBackendUrl [ "write-letter" ] []


configChatUrl =
    absolute [ "config-chat" ] []


backendSpawnDispChatUrl =
    mkBackendUrl [ "spawn-disposable-chat" ] []


backendSpawnPersistChatUrl =
    mkBackendUrl [ "spawn-persistent-chat" ] []


frontendEntranceUrl entranceId =
    absolute [ "persist-chat-entrance", entranceId ] []


backendEntranceUrl entranceId =
    mkBackendUrl [ "persist-chat-entrance", entranceId ] []


frontendChatUrl chatId =
    absolute [ "chat", chatId ] []


backendChatUrl chatId =
    mkBackendUrl [ "chat", chatId ] []


sendMessageUrl chatId =
    mkBackendUrl [ "send-message", chatId ] []
