{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}


module Main where


import           ClassyPrelude
import           Data.Data
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time
import           Network.URI
import           Swish.RDF.Formatter.Turtle
import           System.Console.CmdArgs
import           System.IO (hPutStrLn, stderr)

import Gtd.Formatter
import Gtd.Parser
import Gtd.RDF
import Gtd.Types
import Gtd.Utils


-- | main

printError :: String -> T.Text
printError = ("ERROR: " ++) . T.pack

printGraph :: OutputFormat -> URI -> (TimeZone, Integer) -> Int -> [TodoItem] -> T.Text
printGraph Turtle  p (_, yr) n = formatGraphAsText . todoListToGraph p yr n
printGraph TodoTxt _ (tz, _) _ = formatTodoTxtList tz
printGraph Raw     _ _       _ = T.intercalate "\n" . map (T.pack . show)

doPending :: Bool -> Either String [TodoItem] -> Either String [TodoItem]
doPending False = id
doPending True  = fmap (fmap setPending)


doArchive :: Bool -> Either String [TodoItem] -> Either String [TodoItem]
doArchive False = id
doArchive True  = fmap (fmap archiveItem)


main :: IO ()
main = do
        GtdArgs{..} <- cmdArgs gtdArgs
        case parseURI =<< prefix of
            Just p -> do
                tz <- getCurrentTimeZone
                yr <- getYear . zonedTimeToUTC <$> getZonedTime
                TIO.interact ( either printError (printGraph output p (tz, yr) n)
                             . doArchive archive
                             . doPending pending
                             . parseTodos tz
                             )
            Nothing ->
                hPutStrLn stderr "A valid URI for the --prefix argument is \
                                 \ required. (--help for more information.)"


-- | Command-line processing

data OutputFormat = Raw
                  | Turtle
                  | TodoTxt
                  deriving (Show, Data, Typeable)

data GtdArgs = GtdArgs
             { prefix  :: Maybe String
             , n       :: Int
             , output  :: OutputFormat
             , pending :: Bool
             , archive :: Bool
             } deriving (Show, Data, Typeable)

gtdArgs :: GtdArgs
gtdArgs = GtdArgs
        { prefix  = def &= help "The prefix for IRIs generated."
        , n       = def &= help "The number to begin indexing the items with."
        , output  = TodoTxt &= typ "FORMAT"
                  &= help "The output format (raw, *todotxt*, turtle)."
        , pending = False &= help "Set all active items to pending."
        , archive = False &= help "Archive all completed items as they're read."
        } &= summary "gtd"
          &= details ["Parse a todotxt file into RDF turtle."]

