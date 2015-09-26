module Lib
    ( someFunc
    , Aggregates(..)
    , Events(..)
    , Index(..)
    , Projections(..)
    , EventStore(..)
    ) where

import Control.Concurrent.STM ( STM
                              , atomically
                              )
import Control.Concurrent.STM.TVar ( TVar
                                   , newTVar
                                   , readTVar
                                   , modifyTVar
                                   )

data Aggregates = Aggregates
   deriving (Eq, Ord, Show)

data Event = Event String
    deriving (Eq, Ord, Show)

data Events = Events [Event]
   deriving (Eq, Ord, Show)

data Index = Index
   deriving (Eq, Ord, Show)

data Projections = Projections
   deriving (Eq, Ord, Show)

data EventStore = EventStore Aggregates Events Index Projections
   deriving (Eq, Ord, Show)

-- emit :: EventStore -> Event -> EventStore

type LiveEventStore = TVar EventStore

newEventStore :: STM LiveEventStore
newEventStore = newTVar $ EventStore Aggregates (Events []) Index Projections

addEvent :: Event -> EventStore -> EventStore
addEvent event (EventStore as (Events events) idx ps) = EventStore as ev' idx ps
   where ev' = Events (event : events)

emit :: LiveEventStore -> Event -> IO EventStore
emit es e = atomically $ do
  modifyTVar es (addEvent e)
  readTVar es

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
