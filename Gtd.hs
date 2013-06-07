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
import           Control.Error
import           Control.Monad.Identity
import           Data.Data
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time
import           Network.URI
import           Swish.RDF.Formatter.Turtle
import           System.Console.CmdArgs

import Gtd.Formatter
import Gtd.Parser
import Gtd.RDF
import Gtd.Types
import Gtd.Utils


-- | main

printError :: String -> T.Text
printError = ("ERROR: " ++) . T.pack

doPending :: Bool -> [TodoItem] -> [TodoItem]
doPending False = id
doPending True  = fmap setPending


doArchive :: Bool -> [TodoItem] -> [TodoItem]
doArchive False = id
doArchive True  = fmap archiveItem

main :: IO ()
main = do
        cli <- cmdArgs gtdArgs
        tz  <- getCurrentTimeZone
        yr  <- getYear . zonedTimeToUTC <$> getZonedTime
        TIO.interact (process cli (tz, yr))

process :: GtdArgs -> TimeInfo -> Text -> Text
process cli ti@(tz, _) t =
        either T.pack id . runIdentity . runEitherT $
            generateOutput cli ti =<< transform <$> parse
    where parse     = hoistEither $ parseTodos tz t
          transform = doPending (pending cli) . doArchive (archive cli)

collapse :: Either a a -> a
collapse = either id id

type TimeInfo = (TimeZone, Integer)

generateOutput :: Monad m => GtdArgs -> TimeInfo -> [TodoItem] -> EitherT String m Text
generateOutput Raw{..}     _        items =
        return . T.intercalate "\n" $ map (T.pack . show) items
generateOutput Turtle{..}  (_,  yr) items =
        liftM (formatGraphAsText . listToGraph)
            (hoistEither . note "Invalid URI prefix." . parseURI =<<
                hoistEither (note "You must supply a URI prefix." prefix))
    where listToGraph p = todoListToGraph p yr n items
generateOutput TodoTxt{..} (tz, _ ) items =
        return $ formatTodoTxtList tz items

-- | Command-line processing

data GtdArgs
        = Raw     { n       :: Int
                  , pending :: Bool
                  , archive :: Bool
                  }
        | Turtle  { n       :: Int
                  , pending :: Bool
                  , archive :: Bool
                  , prefix  :: Maybe String
                  }
        | TodoTxt { n       :: Int
                  , pending :: Bool
                  , archive :: Bool
                  }
        deriving (Show, Data, Typeable)

gtdArgs :: GtdArgs
gtdArgs = modes [ TodoTxt { n = def &= help "The number to begin indexing the items with."
                          , pending = False &= help "Set all active items to pending."
                          , archive = False &= help "Archive all completed items as they're read."
                          } &= auto
                , Turtle { n = def &= help "The number to begin indexing the items with."
                         , prefix  = def &= help "The prefix for IRIs generated."
                         , pending = False &= help "Set all active items to pending."
                         , archive = False &= help "Archive all completed items as they're read."
                         }
                , Raw { n = def &= help "The number to begin indexing the items with."
                      , pending = False &= help "Set all active items to pending."
                      , archive = False &= help "Archive all completed items as they're read."
                      }
                ] &= program "gtd"
                  &= summary "Parse a todotxt file into RDF turtle, Haskell data, etc."

