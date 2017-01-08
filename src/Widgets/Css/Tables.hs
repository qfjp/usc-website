{-# LANGUAGE OverloadedStrings #-}
module Widgets.Css.Tables where

import           Clay as C hiding (map, max, tr)

clayCss :: Css
clayCss = do
    splitTableStyle
    tableStyles
    tdStyles

splitTableStyle :: Css
splitTableStyle
  = do
      C.element "div" # byClass "split-container" ? do
          height (pct 100)
          width (pct 75)
          marginLeft auto
          marginRight auto
      C.element "div" # byClass "split-section" ? do
          height (pct 90)
          border none (px 0) (rgb 0 0 0)
          verticalAlign vAlignTop
          C.display inlineBlock
      C.element "div" # byClass "split-subcontent" ? do
          height (pct 90)
          overflow auto

fullTableStyle :: Css
fullTableStyle
  = C.element "table" # byClass "full" ? do
      border solid (px 1) (rgb 221 221 221)
      marginBottom (px 20)
      padding (px 8) (px 8) (px 8) (px 8)

tableStyles :: Css
tableStyles
  = do
      C.element "table" ? do
        width (pct 100)
        maxWidth (pct 100)
        textAlign center
        boxSizing borderBox
        lineHeight (em 1.428)
        borderSpacing (px 0)
        borderCollapse collapse
        backgroundColor transparent
      compactTableStyle
      fullTableStyle

compactTableStyle :: Css
compactTableStyle
  = C.element "table" # byClass "compact" ? do
      border none (px 0) (rgb 0 0 0)
      paddingTop (px 0)
      paddingBottom (px 0)
      marginTop (px 0)
      marginBottom (px 0)

tdStyles :: Css
tdStyles
  = do
      C.element "td" ? do
        lineHeight (em 1.428)
        verticalAlign vAlignTop
        marginRight (pct 100)
      fullTdStyle
      compactTdStyle

compactTdStyle :: Css
compactTdStyle
  = C.element "table" # byClass "compact" |> C.element "tr" |> C.element "td" ? do
      borderTop solid (px 1) (rgb 221 221 221)
      padding (px 4) (px 4) (px 4) (px 4)

fullTdStyle :: Css
fullTdStyle
  = C.element "table" # byClass "full" |> C.element "tr" |> C.element "td" ? do
      border solid (px 1) (rgb 221 221 221)
      padding (px 8) (px 8) (px 8) (px 8)

