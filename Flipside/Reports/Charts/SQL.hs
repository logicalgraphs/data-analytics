module Flipside.Reports.Charts.SQL where

-- we snarf some values and output SQL that is chartable

import Data.List (intercalate)

-- 1HaskellADay imports

import Control.List (weave)
import Control.Presentation (Univ, explode)

chart :: Univ a => [String] -> [a] -> [String]
chart headers = intercalate ["UNION"] . map return . asSQL headers

asSQL :: Univ a => [String] -> [a] -> [String]
asSQL headers = map (enSQLify . zip headers . explode)

enSQLify :: [(String, String)] -> String
enSQLify = ("SELECT " ++) . weave . map marry

marry :: (String, String) -> String
marry (a, b) = b ++ " as " ++ a
