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

import Gtd.Parser
import Gtd.RDF
import Gtd.Utils


-- | main

main :: IO ()
main = do
        GtdArgs{..} <- cmdArgs gtdArgs
        case parseURI =<< prefix of
            Just p -> do
                tz <- getCurrentTimeZone
                yr <- getYear . zonedTimeToUTC <$> getZonedTime
                TIO.interact ( either printError (printGraph output p yr n)
                             . doPending pending
                             . parseTodos tz
                             )
            Nothing ->
                hPutStrLn stderr "A valid URI for the --prefix argument is \
                                 \ required. (--help for more information.)"
    where printError = ("ERROR: " ++) . T.pack

          printGraph Turtle p yr n = formatGraphAsText . todoListToGraph p yr n
          printGraph Raw    _ _  _ = T.intercalate "\n" . map (T.pack . show)

          doPending False = id
          doPending True  = fmap (fmap setPending)

-- | Command-line processing

data OutputFormat = Raw
                  | Turtle
                  deriving (Show, Data, Typeable)

data GtdArgs = GtdArgs
             { prefix  :: Maybe String
             , n       :: Int
             , output  :: OutputFormat
             , pending :: Bool
             } deriving (Show, Data, Typeable)

gtdArgs :: GtdArgs
gtdArgs = GtdArgs
        { prefix  = def &= help "The prefix for IRIs generated."
        , n       = def &= help "The number to begin indexing the items with."
        , output  = Turtle &= typ "FORMAT" &= help "The output format (raw, *turtle*)."
        , pending = False &= help "Set all active items to pending."
        } &= summary "gtd"
          &= details ["Parse a todotxt file into RDF turtle."]

