{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Widgets.Button where
import Reflex
import Reflex.Dom
import qualified Data.Map as Map

onOffButton :: MonadWidget t m => m ()
onOffButton = do
  rec
    let onButtonClass  = Map.singleton "class" "onButton"
    let offButtonClass = Map.singleton "class" "offButton"
    (buttonElement, _) <- elDynAttr' "button" cssClassDynamic $ elDynHtml' "span" textDynamic
    buttonElementClickEvent <- return $ domEvent Click buttonElement
    toggleDynamic <- toggle False buttonElementClickEvent
    cssClassDynamic <- mapDyn (
        \ x -> if x then (
          onButtonClass
        ) else (
          offButtonClass
        )
      ) toggleDynamic
    textDynamic <- mapDyn (
        \ x -> if x then (
          "<i class='fa fa-circle-o-notch' aria-hidden='true'></i>"
        ) else (
          "<i class='fa fa-power-off' aria-hidden='true'></i>"
      )) toggleDynamic
  return ()
