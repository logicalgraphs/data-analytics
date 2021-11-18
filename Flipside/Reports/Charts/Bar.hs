module Flipside.Reports.Charts.Bar where

-- converts a [(String, Int)] to a d3js.org bar-chart

-- https://observablehq.com/@observablehq/plot-bar

-- 1HaskellADay modules

import Control.List (weave)

toBarChart :: [(String, Int)] -> IO ()
toBarChart = putStrLn . ("alphabet=[" ++) . (++ "]") . weave . map toJS

toJS :: (String, Int) -> String
toJS (a,b) = '{':concat ["letter: '", a, "', frequency: ", show b] ++ "}"

{--
>>> toBarChart [("1",2),("3",4)]
{letter: '1', frequency: 2},{letter: '3', frequency: 4}
--}
