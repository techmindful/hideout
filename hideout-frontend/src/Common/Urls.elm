module Common.Urls exposing (..)

import Url.Builder exposing (..)


frontendUrl = "http://localhost:8000"


backendUrl = "http://localhost:8080"


frontendReadLetterUrl = crossOrigin frontendUrl [ "read-letter" ] []


backendReadLetterUrl = crossOrigin backendUrl [ "read-letter" ] []


backendWriteLetterUrl = crossOrigin backendUrl [ "write-letter" ] []


newChatUrl = crossOrigin backendUrl [ "new-chat" ] []


chatUrl chatId = relative [ "chat", chatId ] []


sendMessageUrl chatId = crossOrigin backendUrl [ "send-message", chatId ] []
