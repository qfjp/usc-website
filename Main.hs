{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

import           Clay

import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL

import           Reflex
import           Reflex.Dom

import           Safe               (readMay)


import           Widgets.Css.Tables
import           Widgets.Dom.Tables
import           Widgets.Text

tshow :: Show a => a -> T.Text
tshow = T.pack . show

------- Main
main :: IO ()
main
  = do
      mainWidgetWithHead headElement bodyElement

bodyCss :: Css
bodyCss
  = body ? do
      fontFamily ["Alegreya Sans"] [sansSerif]
      height (pct 100)
      marginLeft auto
      marginRight auto

allCss :: Css
allCss
  = do
      bodyCss
      Widgets.Css.Tables.clayCss
      Widgets.Text.clayCss

headElement :: MonadWidget t m => m ()
headElement
  = do
      el "title" (text "Pade CSCE 355")
      styleSheet "https://fonts.googleapis.com/css?family=Muli"
      styleSheet "https://fonts.googleapis.com/css?family=Alegreya+Sans"
      styleSheet "https://fonts.googleapis.com/css?family=Inconsolata"
      --el "style" (text $ TL.toStrict $ renderWith pretty [] allCss)
      el "style" (text $ TL.unpack $ renderWith compact [] allCss)

styleSheet :: MonadWidget t m => T.Text -> m ()
styleSheet link
  = elAttr "link" (M.fromList [ ("rel", "stylesheet")
                              , ("type", "text/css")
                              , ("href", T.unpack link)
                              ]) $ return ()

bodyElement :: MonadWidget t m => m ()
bodyElement
  = el "div" $ do
      makeNavBar
        NavBar { _navTitle="Main"
               , _sections=["hey", "there", "bud"]
               }
      makeHoverTable "full" (Just "Daniel Padé")
                [ ["Email", "djpade@gmail.com"]
                , ["office", "something"]
                ]
      splitTable [ ("Lectures", [ ["hey"]
                                , ["there"]
                                , ["guy"]
                                , ["what"]
                                , ["is"]
                                , ["happening"]
                                , ["here"]
                                , ["in"]
                                ]
                   )
                 , ("Lectures", [["hey", "there"], ["guy", "what"]])
                 , ("Lectures", [["hey", "there"], ["guy", "what"]])
                 , ("Lectures", [["hey", "there"], ["guy", "what"]])
                 ]

----- Navigation Bar

data NavBar = NavBar { _navTitle :: T.Text, _sections :: [T.Text] }

makeNavBar :: MonadWidget t m => NavBar -> m ()
makeNavBar NavBar {_navTitle=navTitle, _sections=sections}
  = el "nav" $ do
      el "div" . text . T.unpack $ navTitle
      el "div" $ el "ul" $ mapM_ (secNameToElement "li") sections

secNameToElement :: MonadWidget t m => T.Text -> T.Text -> m ()
secNameToElement elTyp
  = el (T.unpack elType) . text . T.unpack

------ Sections
data Section t m = Section { _sname :: T.Text, _content :: m () }
