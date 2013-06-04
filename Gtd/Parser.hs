{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gtd.Parser where

import           Control.Applicative
import           Control.Error
import           Control.Monad.Reader
import           Data.Attoparsec.Text
import           Data.Char (isLetter, isSpace)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time
import           Gtd.Types
import           Network.URI

-- | Parser

type ZonedParser = ReaderT TimeZone Parser

getTZ :: ZonedParser TimeZone
getTZ = ask

parseZone :: TimeZone -> ZonedParser a -> T.Text -> Either String a
parseZone tz p = parseOnly (runReaderT p tz)

parseTodos :: TimeZone -> T.Text -> Either String [TodoItem]
parseTodos = fmap (fmap (fmap parseLabel)) . flip parseZone todoList

todoList :: ZonedParser [TodoItem]
todoList = many' todoItem

todoItem :: ZonedParser TodoItem
todoItem = Todo <$> status
                <*> lift label
                <*> pure Nothing
                <*> pure Nothing
                <*> optional (lift progress)
                <*> optional (lift project)
                <*> many1 (lift context)

parseLabel :: TodoItem -> TodoItem
parseLabel tIn@Todo{..} =
        tIn { todoUri   = join . hush $ parseOnly (find1 uri)      todoLabel
            , todoDate  = join . hush $ parseOnly (find1 monthDay) todoLabel
            , todoLabel = T.strip todoLabel
            }

status :: ZonedParser TodoStatus
status = done <|> lift pri <|> lift active
    where done   = Gtd.Types.Done <$> (lift "x " *> timeStamp <* lift space)
          pri    = Active . Just <$> priority
          active = pure $ Active Nothing

timeStamp :: ZonedParser UTCTime
timeStamp =
        localTimeToUTC <$> getTZ
                       <*> (LocalTime <$> lift (day <* space)
                                      <*> lift time)

day :: Parser Day
day = fromJust <$> pdate
    where pdate =   fromGregorianValid
                <$> decimal <* char '-' <*> decimal <* char '-' <*> decimal

time :: Parser TimeOfDay
time = TimeOfDay <$> decimal <* char ':'
                 <*> decimal
                 <*> fmap fromInteger (option 0 $ char ':' *> decimal)

priority :: Parser Priority
priority = char '(' *> satisfy isLetter <* ") "

label :: Parser T.Text
label = scan (Left 'x') step
    where step (Right _) '+' = Nothing
          step (Right _) '@' = Nothing
          step (Right _) '[' = Nothing
          step _         c | isSpace c = Just (Right c)
                           | otherwise = Just (Left c)

uri :: Parser URI
uri = ptext <$> u
    where u   =   (<>)
              <$> (string "http://" <|> string "https://")
              <*> takeTill isSpace
          ptext = fromJust . parseURI . T.unpack

monthDay :: Parser MonthDay
monthDay =   (,)
         <$> (char '(' *> decimal <* char '/')
         <*> (decimal <* char ')')

progress :: Parser Progress
progress = char '[' *> progress' <* char ']' <* many space
    where progress' = Progress <$> optional (decimal <* char '/')
                               <*> decimal

project :: Parser Project
project = prefixed '+'

context :: Parser Context
context = prefixed '@'

prefixed :: Char -> Parser T.Text
prefixed c = char c *> takeWhile1 (not . isSpace) <* many space

find1 :: Parser a -> Parser (Maybe a)
find1 p = loop
    where loop = (Just <$> p) <|> (anyChar *> loop) <|> pure Nothing


