{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.Bootstrap where

import Prelude hiding (div)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

container :: Html -> Html
container = div ! class_ "container"

row :: Html -> Html
row = div ! class_ "row"
