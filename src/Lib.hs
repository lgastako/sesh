module Lib
    ( someFunc
    , emit
    , installProjection
    ) where

import Control.Concurrent.STM ( STM
                              , atomically
                              )
import Control.Concurrent.STM.TVar ( TVar
                                   , newTVar
                                   , readTVar
                                   , modifyTVar
                                   )

import qualified Data.Text as T
import qualified Data.HashMap as M

type Key = String

data Aggregate = Aggregate
   deriving (Eq, Ord, Show)

type AggMapping = M.Map Key AggValue

data AggValue = AggText T.Text
              | AggInt Int
              | AggMap AggMapping
   deriving (Eq, Ord, Show)

intValue :: AggValue -> Maybe Int
intValue (AggInt n) = (Just n)
intValue _ = Nothing

data Aggregates = Aggregates AggMapping
   deriving (Eq, Ord, Show)

data Event = Event String
    deriving (Eq, Ord, Show)

data Events = Events [Event]
   deriving (Eq, Ord, Show)

data Index = Index
   deriving (Eq, Ord, Show)

type Projection = (Aggregates -> Event -> Aggregates)

data Projections = Projections [Projection]

instance Show Projections
   where show _ = "<ProjectionFn>"

data EventStore = EventStore Aggregates Events Index Projections
    deriving (Show)

type LiveEventStore = TVar (EventStore)

newEventStore :: STM LiveEventStore
newEventStore = newTVar $ EventStore (Aggregates M.empty)
                                     (Events [])
                                     Index
                                     (Projections [])

addEvent :: Event -> EventStore -> EventStore
addEvent e (EventStore as (Events es) idx ps) = EventStore as ev' idx ps
   where ev' = Events (e:es)

addProjection :: Projection -> EventStore -> EventStore
addProjection p (EventStore as es idx (Projections ps)) = EventStore as es idx ps'
   where ps' = Projections (p:ps)

runProjection :: Projection -> EventStore -> EventStore
runProjection p (EventStore as (Events es) idx ps) = EventStore as' (Events es) idx ps
  where as' = foldl p as es

installProjection :: LiveEventStore -> Projection -> IO EventStore
installProjection es p = atomically $ do
  modifyTVar es (addProjection p . runProjection p)
  readTVar es

emit :: LiveEventStore -> Event -> IO EventStore
emit es e = atomically $ do
  modifyTVar es (addEvent e)
  readTVar es

inc :: AggValue -> AggValue
inc (AggInt n) = AggInt (succ n)
inc _          = AggInt 1

-- totalEventCount :: Projection
totalEventCount :: Aggregates -> Event -> Aggregates
-- totalEventCount (Aggregates am) e = Aggregates am'
--    where am' = (M.insert k v am)
--          v = succ $ M.lookupDefault 0 k am
         -- k = "total-event-count"
totalEventCount (Aggregates as) e = Aggregates $ M.insert "total-event-count" (AggInt 1) as

someFunc :: IO ()
someFunc = do
  store <- atomically newEventStore
  storeVal <- atomically $ readTVar store
  putStrLn $ "empty event store: " ++ (show storeVal)
  emit store (Event "foo")
  emit store (Event "bar")
  emit store (Event "baz")
  storeVal' <- emit store (Event "bif")
  putStrLn $ "post emit: " ++ (show storeVal')
  x <- installProjection store totalEventCount
  return ()
