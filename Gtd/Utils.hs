{-# LANGUAGE OverloadedStrings #-}


module Gtd.Utils where


import Gtd.Types


setPending :: TodoItem -> TodoItem
setPending t@Todo{ todoStatus = (Active _) } = t { todoStatus = Pending }
setPending t                                 = t

archiveItem :: TodoItem -> TodoItem
archiveItem t@Todo{ todoStatus = (Done date) } = t { todoStatus = Archived date }
archiveItem t                                  = t

