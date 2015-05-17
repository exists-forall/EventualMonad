module Main where

import qualified Eventual as Eventual -- imported qualified to make it clear what functions are from Eventual.
import qualified Data.Map as Map

op1 :: Eventual.Eventual (Map.Map String Int) ()
op1 = do
    fooVal <- Eventual.waitGet $ Eventual.eventualKey "foo"
    Eventual.update $ Eventual.mapUpdate "bar" (fooVal * fooVal)

op2 :: Eventual.Eventual (Map.Map String Int) ()
op2 = Eventual.update $ Eventual.mapUpdate "foo" 4

main = do
    let state0 = Eventual.eventualState Map.empty
    let state1 = Eventual.runEventual op1 state0
    putStrLn "After running op1:"
    print $ Eventual.storageNow state1 -- prints: fromList []
    let state2 = Eventual.runEventual op2 state1
    putStrLn "After running op2:"
    print $ Eventual.storageNow state2 -- prints: fromList [("bar",16),("foo",4)]
