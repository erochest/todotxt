{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gtd.RDF where

import           Control.Applicative
import           Data.Char (toLower)
import           Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time
import           Network.URI
import           Swish.Namespace
import           Swish.QName
import           Swish.RDF.Graph
import           Swish.RDF.Vocabulary.RDF
import           Swish.RDF.Vocabulary.XSD
import           Text.Printf
import Gtd.Types


todoToTriples :: NamespaceMap
              -> Integer
              -> (Int, RDFArcSet)
              -> TodoItem
              -> (Int, RDFArcSet)
todoToTriples nss year (n, arcs) Todo{..} = (n + 1, arcs')
    where dtime   = doneTime todoStatus
          s       = M.lookup (Just "s")   nss
          upri    = M.lookup (Just "pri") nss
          gtdName = makeName nss "gtd"

          item    = makeStringName nss "todo" $ printf "item%04d" n

          titem   = gtdName "todoitem"
          tstatus = gtdName "status"
          turl    = gtdName "itemurl"
          tpri    = gtdName "priority"
          tdone   = gtdName "donedate"
          ddate   = gtdName "duedate"
          ctx     = gtdName "context"
          proj    = gtdName "project"
          prog    = gtdName "progress"
          progD   = gtdName "progressDone"
          progE   = gtdName "progressEstimate"


          insert' = flip maybeInsert
          arcs'   = insert' (flip toRDFTriple rdfType <$> item <*> titem)
                  . insert' (flip labelRdf todoLabel <$> item)
                  . insert' (statusRdf <$> tstatus <*> s <*> pure (localStatus todoStatus) <*> item)
                  . insert' (priRdf <$> item <*> tpri <*> upri <*> localPri todoStatus)
                  . insert' (toRDFTriple <$> item <*> turl <*> todoUri)
                  . insert' (toRDFTriple <$> item <*> tdone <*> dtime)
                  . insert' (toRDFTriple <$> item <*> ddate <*> fmap (dueDate year dtime) todoDate)
                  . insert' (toRDFTriple <$> item <*> proj <*> (makeTextName nss "p" =<< todoProject))
                  . flip (foldl' maybeInsert) (progressRdf item n prog progD progE todoProgress)
                  $ foldl' maybeInsert arcs [ toRDFTriple <$> item <*> ctx <*> makeTextName nss "c" context
                                            | context <- todoContexts
                                            ]

progressRdf :: Maybe ScopedName -> Int -> Maybe ScopedName -> Maybe ScopedName
            -> Maybe ScopedName -> Maybe Progress -> [Maybe RDFTriple]
progressRdf _ _ _ _ _ Nothing = []
progressRdf item n prog progD progE (Just (Progress done estimate)) =
        [ toRDFTriple <$> item <*> prog <*> pure parent
        , toRDFTriple parent <$> progD <*> done
        , flip (toRDFTriple parent) estimate <$> progE
        ]
        where parent = Blank $ show n ++ "progress"

quickGraph :: NamespaceMap -> RDFArcSet -> RDFGraph
quickGraph nss arcs = emptyRDFGraph { namespaces = nss
                                    , statements = arcs
                                    }

makeNamespaces :: URI -> NamespaceMap
makeNamespaces prefix = M.fromList [ (Just "gtd",  extendPath prefix "/gtd#")
                                   , (Just "todo", extendPath prefix "/todo#")
                                   , (Just "p",    extendPath prefix "/project#")
                                   , (Just "c",    extendPath prefix "/context#")
                                   , (Just "s",    extendPath prefix "/status#")
                                   , (Just "pri",  extendPath prefix "/priority#")
                                   , getNamespaceTuple namespaceRDF
                                   , getNamespaceTuple namespaceRDFS
                                   , getNamespaceTuple namespaceXSD
                                   ]

makeName :: NamespaceMap -> T.Text -> LName -> Maybe ScopedName
makeName nss pre lname = flip (makeScopedName mpre) lname <$> M.lookup mpre nss
    where mpre = Just pre

makeTextName :: NamespaceMap -> T.Text -> T.Text -> Maybe ScopedName
makeTextName nss pre tname = newLName tname >>= makeName nss pre

makeStringName :: NamespaceMap -> T.Text -> String -> Maybe ScopedName
makeStringName nss pre = makeTextName nss pre . T.pack

todoListToGraph :: URI -> Integer -> Int -> [TodoItem] -> RDFGraph
todoListToGraph prefix year n =
          quickGraph nss
        . snd
        . foldl' (todoToTriples nss year) (n, S.empty)
    where nss   = makeNamespaces prefix

maybeInsert :: Ord a => S.Set a -> Maybe a -> S.Set a
maybeInsert s (Just a) = S.insert a s
maybeInsert s Nothing  = s

-- | RDF Stuff

instance ToRDFLabel T.Text where
        toRDFLabel = toRDFLabel . T.unpack

localPri :: TodoStatus -> Maybe LName
localPri (Active (Just p)) = newLName . T.singleton $ toLower p
localPri _                 = Nothing

priRdf :: (ToRDFLabel s) => s -> ScopedName -> URI -> LName -> RDFTriple
priRdf s p tpri = toRDFTriple s p . makeScopedName (Just "pri") tpri

extendPath :: URI -> String -> URI
extendPath u@URI{..} p = u { uriPath = uriPath ++ p }

localStatus :: TodoStatus -> LName
localStatus (Active _)    = "active"
localStatus Pending       = "pending"
localStatus (Gtd.Types.Done _) = "done"
localStatus (Archived _)  = "archived"

statusRdf :: ToRDFLabel a => ScopedName -> URI -> LName -> a -> RDFTriple
statusRdf gtd p lname s = toRDFTriple s gtd $ makeScopedName (Just "s") p lname

labelRdf :: ToRDFLabel a => a -> T.Text -> RDFTriple
labelRdf item = toRDFTriple item rdfsLabel . Lit

doneTime :: TodoStatus -> Maybe UTCTime
doneTime (Gtd.Types.Done t) = Just t
doneTime (Archived t)  = Just t
doneTime (Active _)    = Nothing
doneTime Pending       = Nothing

dueDate :: Integer -> Maybe UTCTime -> MonthDay -> Day
dueDate year doneDate (month, day) =
        fromGregorian (maybe year getYear doneDate) month day

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

getYear :: UTCTime -> Integer
getYear = fst3 . toGregorian . utctDay

