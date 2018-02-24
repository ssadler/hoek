{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Komodo.Crypto.B16Keys where

import           Control.Monad (guard)
import           Data.Aeson 
import           Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)


parseB16 :: Text -> Parser BS.ByteString
parseB16 = maybe (fail "Invalid hex string") pure . decodeHex . encodeUtf8


toB16 :: BS.ByteString -> Text
toB16 = decodeUtf8 . encodeHex


encodeHex :: BS.ByteString -> BS.ByteString
encodeHex = B16.encode

-- | Decode hexadecimal 'ByteString'. This function can fail if the string
-- contains invalid hexadecimal (0-9, a-f, A-F) characters
decodeHex :: BS.ByteString -> Maybe BS.ByteString
decodeHex bs =
    let (x, b) = B16.decode bs
    in guard (b == BS.empty) >> return x


