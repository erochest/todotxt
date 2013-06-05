
module Gtd.Types where


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

data Progress = Progress (Maybe Int) Int
              deriving (Show)

data TodoItem = Todo 
              { todoStatus   :: TodoStatus
              , todoLabel    :: T.Text
              , todoUri      :: Maybe URI
              , todoDate     :: Maybe MonthDay
              , todoProgress :: Maybe Progress
              , todoProject  :: Maybe Project
              , todoContexts :: [Context]
              } deriving (Show)

