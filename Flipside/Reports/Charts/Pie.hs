module Flipside.Reports.Charts.Pie where

-- converts a [(String, Int)] to a d3js.org pie-chart

-- 1HaskellADay modules

import Control.List (weave)

toPieChart :: [(String, Int)] -> IO ()
toPieChart = putStrLn . weave . map toJS

toJS :: (String, Int) -> String
toJS (a,b) = concat ["'", a, "': ", show b]
