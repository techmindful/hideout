module Utils where

import           Text.Read ( readMaybe )


readPosInt :: String -> Maybe Int
readPosInt str =
  case readMaybe str of
    Just int ->
      if int >= 1 then Just int
      else Nothing
    Nothing -> Nothing
