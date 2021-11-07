module Flipside.Bounties.Ingest where

-- we read JSON from an endpoint and return rows of data for analytical goodness!

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Network.HTTP.Conduit

fetchWith :: FromJSON a => FilePath -> (ByteString -> Either String [a])
                        -> IO [a]
fetchWith url f = simpleHttp url >>= reportErrors . f

reportErrors :: Either String [a] -> IO [a]
reportErrors (Left err) = error err
reportErrors (Right ans) = return ans

-- a bare-level (pass-thru) parser

decodeObjs :: ByteString -> Either String [Value]
decodeObjs = eitherDecode

summerUrl :: FilePath
summerUrl = "https://api.flipsidecrypto.com/api/v2/queries/"
         ++ "abe74b5e-884a-4d67-a19f-a5b6fca6038f/data/latest"

version0 :: IO ()
version0 = fetchWith summerUrl decodeObjs >>=
           putStrLn . ("There are " ++) . (++ " rows of data.") . show . length

{--
>>> version0 
There are 11133 rows of data.
--}
