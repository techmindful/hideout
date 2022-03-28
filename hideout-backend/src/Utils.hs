module Utils where

import qualified Data.Text as Text
import           Data.Text ( Text )
import           Text.Read ( readMaybe )


readPosInt :: Text -> Maybe Int
readPosInt text =
  case readMaybe ( Text.unpack text ) of
    Just int ->
      if int >= 1 then Just int
      else Nothing
    Nothing -> Nothing
