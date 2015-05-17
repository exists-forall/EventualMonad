{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, GADTs #-}

module Eventual
	(Wait
	,EventualGetter
	,EventualUpdate
	,TriggerID
	,tryGetNow
	,updateNow
	,EventualMapKey
	,EventualMapUpdate
	,eventualKey
	,mapUpdate
	,Eventual
	,EventualState
	,eventualState
	,storageNow
	,waitGet
	,update
	,runEventual
	,allTasksComplete)
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

class (Ord (TriggerID w)) => Wait w where
	type EventualGetter w :: * -> *
	type EventualUpdate w :: *
	type TriggerID w :: *
	
	tryGetNow :: EventualGetter w v -> w -> Either (TriggerID w) v
	updateNow :: EventualUpdate w   -> w -> (w, TriggerID w)

data EventualMapKey k v a where
	EventualMapKey :: k -> EventualMapKey k v v

eventualKey :: k -> EventualMapKey k v v
eventualKey = EventualMapKey

data EventualMapUpdate k v
	= EventualMapUpdate k v

mapUpdate :: k -> v -> EventualMapUpdate k v
mapUpdate = EventualMapUpdate

instance (Ord k) => Wait (Map.Map k v) where
	type EventualGetter (Map.Map k v) = EventualMapKey k v
	type EventualUpdate (Map.Map k v) = EventualMapUpdate k v
	type TriggerID (Map.Map k v) = k
	
	tryGetNow (EventualMapKey k) m =
		case (Map.lookup k m) of
			Just val -> Right val
			Nothing  -> Left  k
	
	updateNow (EventualMapUpdate k v) m = (Map.insert k v m, k)

data EventualTask w a where
	EventualTask :: (Wait w) => TriggerID w -> Eventual w a -> EventualTask w a

data Eventual w a
	= Eventual (EventualState w -> (EventualState w, Either (EventualTask w a) a))

data EventualState w where
	EventualState :: (Wait w) => w -> (Map.Map (TriggerID w) [Eventual w ()]) -> EventualState w

eventualState :: (Wait w) => w -> EventualState w
eventualState storage = EventualState storage Map.empty

storageNow :: EventualState w -> w
storageNow (EventualState storage _) = storage

waitGet :: (Wait w) => EventualGetter w v -> Eventual w v
waitGet g = cont where
	cont = Eventual $ \state ->
		let result = case tryGetNow g (storageNow state) of
			Left  trigger -> Left $ EventualTask trigger cont
			Right val     -> Right val
		in (state, result)

update :: (Wait w) => EventualUpdate w -> Eventual w ()
update u = Eventual $ \state ->
	let
		(storage', trigger) = updateNow u (storageNow state)
		EventualState _ tasks = state
		toTrigger = fromMaybe [] $ Map.lookup trigger tasks
		state' = EventualState storage' $ Map.delete trigger tasks
		state'' = foldr runEventual state' toTrigger
	in (state'', Right ())

runEventual :: Eventual w () -> EventualState w -> EventualState w
runEventual (Eventual f) state =
	let (state', result) = f state
	in case result of
		Left  (EventualTask trigger cont) ->
			let
				EventualState storage tasks = state'
				taskList = fromMaybe [] $ Map.lookup trigger tasks
				taskList' = cont : taskList
				tasks' = Map.insert trigger taskList' tasks
			in EventualState storage tasks'
		Right () -> state'

allTasksComplete :: EventualState w -> Bool
allTasksComplete state = 
	let EventualState _ tasks = state
	in Map.null tasks

instance Monad (Eventual w) where
	return x = Eventual $ \state -> (state, Right x)
	
	(Eventual f1) >>= f2 = Eventual $ \state ->
		case f1 state of
			(state', Left (EventualTask trigger cont)) -> (state', Left (EventualTask trigger $ cont >>= f2))
			(state', Right val) ->
				let Eventual f3 = f2 val
				in f3 state'
