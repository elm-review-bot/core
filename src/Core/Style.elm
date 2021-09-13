module Core.Style exposing (style)

import Css exposing (..)
import Css.Elements exposing (body)
import Css.File as CF
import Html
import Core.Style.StyleSheet as SS

{-BEM naming convention is used-}



style = 
    Html.node "style" [] [([SS.css] |> CF.compile |> .css |> Html.text)]


