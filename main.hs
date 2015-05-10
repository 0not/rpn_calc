{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import Reflex.Dom.Widget.Input
import Reflex.Dom.Class
import qualified Data.Map as Map
import Data.Monoid ((<>))

import Safe (readMay)

import Calc (calculate)

stylesheet :: MonadWidget t m => String -> m ()
stylesheet s = elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank

scriptSrc :: MonadWidget t m => String -> m ()
scriptSrc s = elAttr "script" ("type" =: "javascript" <> "src" =:  s) blank

metaEdge :: MonadWidget t m => m ()
metaEdge = elAttr "meta" ("http-equiv" =: "X-UA-Compatible" <> "content" =: "IE=edge") blank

metaViewport :: MonadWidget t m => String -> m ()
metaViewport s = elAttr "meta" ("name" =: "viewport" <> "content" =: s) blank

aClass :: MonadWidget t m => String -> String -> String -> m ()
aClass c h t = elAttr "a" ("class" =: c <> "href" =: h) $ text t

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
        rec divClass "stack" $ elAttr "ol" ("class" =: "list" <> "reversed" =: "reversed")  $ simpleList result $ \t -> el "li" $ dynText t
            ti <- textInput (TextInputConfig "text" "1 2 +" never $ constDyn ("class" =: "form-control"))
            result <- mapDyn (map show . calculate) $ _textInput_value ti
        divClass "notes" $ do
            el "p" $ text "Supported operations: + - * / tan sin cos atan asin acos d2r r2d"
            el "p" $ text "e.g. To calculate the 0.5 * cos(2/3) enter: 0.5 2 3 / cos *"
