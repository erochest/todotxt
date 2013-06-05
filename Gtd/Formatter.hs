{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Gtd.Formatter where


import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Builder.Int
import           Data.Time
import           System.Locale (defaultTimeLocale)

import Gtd.Types


timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M"

formatTodoTxtList :: TimeZone -> [TodoItem] -> T.Text
formatTodoTxtList tz = renderBuilder . foldr (buildTodoTxt tz) mempty

formatTodoTxt :: TimeZone -> TodoItem -> T.Text
formatTodoTxt tz = renderBuilder . flip (buildTodoTxt tz) mempty

buildTodoTxt :: TimeZone -> TodoItem -> TB.Builder -> TB.Builder
buildTodoTxt tz Todo{..} b =
        status todoStatus          <>
            (label todoLabel       <>
            (progress todoProgress <>
            (project todoProject   <>
            (contexts todoContexts <>
            ("\n"                  <> b)))))

    where status (Active (Just p)) = "(" <> TB.fromText (T.singleton p) <> ") "
          status (Active Nothing)  = mempty
          status Pending           = mempty
          status (Done time)       = "x " <> timeBuilder tz time <> " "
          status (Archived time)   = "x " <> timeBuilder tz time <> " "

          label l = TB.fromText l <> " "

          progress (Just (Progress (Just i) j)) = "[" <> decimal i <> "/" <> decimal j <> "] "
          progress (Just (Progress Nothing  j)) = "[" <> decimal j <> "] "
          progress Nothing                      = mempty

          project (Just p) = "+" <> TB.fromText p <> " "
          project Nothing  = mempty

          contexts = foldr context " "
          context c b' = "@" <> (TB.fromText c <> (" " <> b'))

timeBuilder :: TimeZone -> UTCTime -> TB.Builder
timeBuilder tz =
        TB.fromString . formatTime defaultTimeLocale timeFormat . utcToZonedTime tz

renderBuilder :: TB.Builder -> T.Text
renderBuilder = TL.toStrict . TB.toLazyText

