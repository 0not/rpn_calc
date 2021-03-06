# Simple RPN Calculator

I made this app to help me familiarize myself with [reflex](https://github.com/ryantrinkle/try-reflex).

See the [demo](http://0not.github.io/rpn_calc/main.jsexe/index.html).


## Reflex Code

```haskell
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import Reflex.Dom.Widget.Input
import Reflex.Dom.Class
import qualified Data.Map as Map
import Safe (readMay)

import Calc (calculate)

stylesheet :: MonadWidget t m => String -> m ()
stylesheet s = elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

scriptSrc :: MonadWidget t m => String -> m ()
scriptSrc s = elAttr "script" (Map.fromList [("type", "javascript"), ("src", s)]) $ return ()

metaEdge :: MonadWidget t m => m ()
metaEdge = elAttr "meta" (Map.fromList [("http-equiv", "X-UA-Compatible"), ("content", "IE=edge")]) $ return ()

metaViewport :: MonadWidget t m => String -> m ()
metaViewport s = elAttr "meta" (Map.fromList [("name", "viewport"), ("content", s)]) $ return ()

aClass :: MonadWidget t m => String -> String -> String -> m ()
aClass c h t = elAttr "a" (Map.fromList [("class", c), ("href", h)]) $ text t

headSection = do 
    metaEdge
    metaViewport "width=device-width, initial-scale=1"
    stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-material-design/0.3.0/css/material-fullpalette.css"
    stylesheet "../my_style.css" 
    scriptSrc "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js" 

navbar = elClass "nav" "navbar" $ do
    divClass "container-fluid" $ do
        divClass "navbar-header" $ do
            aClass "navbar-brand" "#" "RPN Calculator"

main = mainWidgetWithHead headSection $ do
    navbar
    divClass "container" $ do
        rec divClass "stack" $ simpleList result $ \t -> el "div" $ dynText t
            ti <- textInput (TextInputConfig "text" "1 2 +" never $ constDyn $ Map.fromList [("class","form-control")])
            result <- mapDyn (map show . calculate) $ _textInput_value ti
        divClass "notes" $ do
            el "p" $ text "Supported operations: + - * / tan sin cos atan asin acos d2r r2d"
            el "p" $ text "e.g. To calculate the 0.5 * cos(2/3) enter: 0.5 2 3 / cos *"
```

## Calculator

```haskell
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
```

