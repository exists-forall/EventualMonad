{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}

-- This example demonstrates how to create your own
-- instances of the Wait typeclass, to implement custom
-- eventual contexts.

module Main where

import Eventual

data MyContext = MyContext (Maybe String) (Maybe Int) deriving (Show)

data MyContextEventualGetter v where
	EventualFirst :: MyContextEventualGetter String
	EventualSecond :: MyContextEventualGetter Int

data MyContextEventualUpdate
	= UpdateFirst String
	| UpdateSecond Int

data MyContextTrigger
	= TriggerFirst
	| TriggerSecond deriving (Ord, Eq)

instance Wait MyContext where
	type EventualGetter MyContext = MyContextEventualGetter
	type EventualUpdate MyContext = MyContextEventualUpdate
	type TriggerID MyContext = MyContextTrigger
	
	tryGetNow EventualFirst (MyContext (Just first) _) = Right first
	tryGetNow EventualFirst (MyContext Nothing _) = Left TriggerFirst
	
	tryGetNow EventualSecond (MyContext _ (Just second)) = Right second
	tryGetNow EventualSecond (MyContext _ Nothing) = Left TriggerSecond
	
	updateNow (UpdateFirst first) (MyContext _ second)  = (MyContext (Just first) second, TriggerFirst)
	updateNow (UpdateSecond second) (MyContext first _) = (MyContext first (Just second), TriggerSecond)

op1 :: Eventual MyContext ()
op1 = do
	meaningOfLife <- waitGet $ EventualSecond
	let message = "The meaning of life is: " ++ (show meaningOfLife)
	update $ UpdateFirst message

op2 :: Eventual MyContext ()
op2 = update $ UpdateSecond 42

main = do
	let state0 = eventualState $ MyContext Nothing Nothing
	let state1 = runEventual op1 state0
	putStrLn "After op1:"
	print $ storageNow state1 -- prints: MyContext Nothing Nothing
	let state2 = runEventual op2 state1
	putStrLn "After op2:"
	print $ storageNow state2 -- prints: MyContext (Just "The meaning of life is: 42") (Just 42)
