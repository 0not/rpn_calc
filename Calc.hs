-- A simple RPN calculator
--
module Calc where

import Control.Monad
import Text.Read (readMaybe)
import Safe (tailSafe, headMay)

calculate :: String -> [Double]
calculate ""         = []
calculate expression = filter (not . isNaN) (reverse (foldl rpn [] (words expression)))
    where 
        rpn xs op
            | op == "+"    = (y + x) : ys 
            | op == "-"    = (y - x) : ys 
            | op == "*"    = (y * x) : ys 
            | op == "/"    = (y / x) : ys 
            | op == "tan"  = (tan x)  : y : ys 
            | op == "sin"  = (sin x)  : y : ys 
            | op == "cos"  = (cos x)  : y : ys 
            | op == "atan" = (atan x) : y : ys 
            | op == "asin" = (asin x) : y : ys 
            | op == "acos" = (acos x) : y : ys 
            | op == "d2r"  = (x * pi / 180) : y : ys -- degrees to radians
            | op == "r2d"  = (x * 180 / pi) : y : ys -- degrees to radians
            | otherwise    = case readMaybe op of
                                Just x -> x : xs
                                Nothing -> xs
                where 
                    x  = case headMay xs of 
                            Just x -> x
                            Nothing -> acos(2) -- Set to NaN
                    y  = case headMay $ tailSafe xs of 
                            Just x -> x
                            Nothing -> acos(2) -- Set to NaN
                    ys = tailSafe $ tailSafe xs 

