module Flipside.Control.Scanner where

-- Scan in JSON objects from an endpoint

import Data.Aeson (FromJSON, Value, eitherDecode)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)

import Network.HTTP.Conduit (simpleHttp)

fetchWith :: FromJSON a => FilePath -> (ByteString -> Either String [a])
                        -> IO [a]
fetchWith url f = simpleHttp url >>= reportErrors . f

reportErrors :: Either String [a] -> IO [a]
reportErrors (Left err) = error err
reportErrors (Right ans) = return ans

-- a bare-level (pass-thru) parser

decodeObjs :: ByteString -> Either String [Map String Value]
decodeObjs = eitherDecode
