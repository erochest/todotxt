
module Gtd.Types where


import           Control.Monad.Reader
import           Data.Attoparsec.Text
import qualified Data.Text as T
import           Data.Time
import           Network.URI


data TodoStatus = Active (Maybe Priority)
                | Pending
                | Done UTCTime
                | Archived UTCTime
                deriving (Show)

type Project  = T.Text
type Context  = T.Text
type Priority = Char
type MonthDay = (Int, Int)

data TodoItem = Todo 
              { todoStatus   :: TodoStatus
              , todoLabel    :: T.Text
              , todoUri      :: Maybe URI
              , todoDate     :: Maybe MonthDay
              , todoProject  :: Maybe Project
              , todoContexts :: [Context]
              } deriving (Show)

-- | Parser

type ZonedParser = ReaderT TimeZone Parser
