{-# LANGUAGE OverloadedStrings  #-}
module Widgets.Text where

import qualified Data.Text as T
import Reflex hiding (combineDyn, mapDyn)
import Reflex.Dom hiding (combineDyn, mapDyn)
import Clay as C hiding (tr, map, max)

clayCss :: Css
clayCss
  = do
      subheaderCss

subheaderCss :: Css
subheaderCss
  = C.span # byClass "subheader" ? do
      fontFamily ["Muli"] [sansSerif]
      fontSize (px 22)

formatEmail :: MonadWidget t m => T.Text -> m ()
formatEmail email
  = do
      return ()
