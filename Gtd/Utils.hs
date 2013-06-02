{-# LANGUAGE OverloadedStrings #-}


module Gtd.Utils where


import Gtd.Types


setPending :: TodoItem -> TodoItem
setPending t@Todo{ todoStatus = (Active _) } = t { todoStatus = Pending }
setPending t                                 = t

