{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Widgets.Dom.Tables where

import           Widgets.Css.Tables

import           Control.Monad      (when)
import           Data.Functor.Misc  (WrapArg (..))
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe, isJust)
import qualified Data.Text          as T
import           Reflex
import           Reflex.Dom

--TODO: style attributes seem to be merging incorrectly

mapToCss :: M.Map T.Text T.Text -> M.Map T.Text T.Text
mapToCss style
  = "style" =: M.foldrWithKey appendPairToCss "" style
  where pairToCssPair k v
          = T.snoc k ':' `T.append` v
        appendPairToCss k v css
          = T.snoc (pairToCssPair k v) ';' `T.append` css

txtToStrMap :: M.Map T.Text T.Text -> M.Map String String
txtToStrMap
  = M.mapKeys T.unpack . M.map T.unpack

splitTable :: MonadWidget t m => [(T.Text, [[T.Text]])] -> m ()
splitTable sections
  = elAttr "div" ("class" =: "split-container") $
      mapM_ toScrollTable sections
  where width :: Int
        width
          = floor $
              (100 / ((fromIntegral . length) sections) :: Double)
        widthStr :: String
        widthStr = (\x -> x ++ "%") . show $ width
        toScrollTable :: MonadWidget t m => (T.Text, [[T.Text]]) -> m ()
        toScrollTable (title, contents)
          = elAttr "div" (("class" =: "split-section") `M.union`
                          ("width" =: widthStr)) $ do
              makeHoverTable "compact subheader" (Just title) []
              scrollContents contents
        scrollContents :: MonadWidget t m => [[T.Text]] -> m ()
        scrollContents contents
          = elAttr "div" ("class" =: "split-subcontent") $
                   makeHoverTable "compact" Nothing contents

makeHoverTable :: MonadWidget t m => T.Text -> Maybe T.Text -> [[T.Text]] -> m ()
makeHoverTable clazz title contents
  = table' ("class" =: clazz) $ do
      let maxWidth = foldr (max . length) 0 contents
      when (isJust title) $ titleHoverRow maxWidth (fromMaybe "" title)
      mapM_ (\x -> listToTr (length x) maxWidth x) contents
  where listToTr :: MonadWidget t m => Int -> Int -> [T.Text] -> m ()
        listToTr width maxWidth xs
          = trHover $
              -- TODO: Modify colspan attribute
              let colspan = if length xs == 1
                            then "colspan" =: (T.pack . show $ maxWidth)
                            else M.empty :: M.Map T.Text T.Text
                  xs' = map T.unpack xs
              in mapM_ (td' (("width" =: widthToPct width) <> colspan) . text) xs'
        titleHoverRow :: MonadWidget t m => Int -> T.Text -> m ()
        titleHoverRow width title
          = trHover $
              td' ("colspan" =: (T.pack . show $ width)) $
                  elAttr "span" ("class" =: "subheader") . text . T.unpack $ title

widthToPct :: Int -> T.Text
widthToPct w
  = let pct = floor $ (1 / fromIntegral w) * 100
    in (T.pack . show) pct `T.append` "%"
---
-- Element Functions
---
td :: MonadWidget t m => m () -> m ()
td
  = td' M.empty

td' :: MonadWidget t m => M.Map T.Text T.Text -> m () -> m ()
td' txtMap
  = elAttr "td" (txtToStrMap txtMap)

table' :: MonadWidget t m => M.Map T.Text T.Text -> m () -> m ()
table' txtMap
  = elAttr "table" (txtToStrMap txtMap)

trHover :: MonadWidget t m => m () -> m ()
trHover
  = trHover' M.empty

trHover' :: MonadWidget t m => M.Map T.Text T.Text -> m a -> m ()
trHover' attrs child
  = do
      let mergedAttr = attrs
      rec
        let enterColor = mapToCss $ M.fromList [("background-color", "#F5F5F5")]
            leaveColor = mapToCss $ M.fromList [("background-color", "#FFFFFF")]
            enterCss = txtToStrMap $ mergedAttr <> enterColor :: M.Map String String
            leaveCss = txtToStrMap $ mergedAttr <> leaveColor :: M.Map String String
            enterEvent = domEvent Mouseenter element -- :: Event t a
            leaveEvent = domEvent Mouseleave element -- :: Event t b
        (element, _) <- elDynAttr' "tr" cssClassDynamic child
        tEnter <- toggle False enterEvent
        tLeave <- toggle False leaveEvent
        let switchCss :: Bool -> Bool -> M.Map String String
            switchCss enter leave
              = if enter == leave then leaveCss else enterCss
        cssClassDynamic <- combineDyn switchCss tEnter tLeave
      return ()

(<>) :: Ord a => M.Map a T.Text -> M.Map a T.Text -> M.Map a T.Text
(<>)
  = M.unionWith (\x y -> x `T.append` ";" `T.append` y)
