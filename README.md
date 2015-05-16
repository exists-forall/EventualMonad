# The `Eventual` Monad

This is a very small, very simple, and in my opnion quite useful library for automatically managing runtime data dependencies in Haskell.

## What does that mean?

In some domains, like when writing compilers, it's common to run into annoying situations where you need to delay processing of some data until some other data has already been processed.  For example, let's think about the steps needed to compile the following program (written in [Blub](http://c2.com/cgi/wiki?BlubParadox)):

	func foo() -> Int {
		print(bar(1 + 1));
	}
	
	func bar(x: Int) -> Int {
		return x + x;
	}

When compiling the body of `foo`, the compiler must, among other things, perform type checking.  This requires knowing the argument and return types of `bar`, to ensure that `print(bar(1 + 1))` is semantically valid.  However, in a naive implementation, the compiler would have not yet reached or processed the definition of `bar` at this point in the file, because it comes after `foo`, and would therefore not yet know `bar`'s argument or return types.

To fix this, nearly all compilers do multiple passes through the source code they're processing, building up the information for the next pass on a whole-program level every time.  In this case, the compiler would first process `foo` and `bar`'s type signatures, then type-check their bodies in a second pass using that knowledge.  For handling more complex features of the language you're compiling, like templates/generics, managing all of these passes and figuring out their proper ordering can be quite irritating, and involve a lot of boilerplate code and intermediate data structures.

## `Eventual` saves the day!

**The `Eventual` monad let's you perform operations which are allowed to depend on elements of a data structure have not yet been defined.**  When you request an element which has not yet been defined (such as, in the above case, the type signature of `bar`), the monad automatically "queues up" the code which requested that element, suspending its execution.  When the data does finally become available, all of the suspended operations which depended on it are automatically executed.  When compiling the program above, the operation to type-check `foo` would run as soon as `bar`'s type signature was set.

## A simple example

Let's take a look at how this actually works in practice.  Keep in mind that this is a *Haskell* library, and this example assumes a familiarity with the language.  If you don't know Haskell, but this all sounds very useful to you, I reccomend that you learn it!  Writing code in Haskell is a great time, and it lets you do all sorts of neat little things like this.  I would not reccomend trying to implement `Eventual` in C++ (although in a langauge with coroutines, like Lua, you might be able to do it).

Without further ado, here's about the simplest program you can write using `Eventual`:

```Haskell
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
	print $ Eventual.storageNow state1
	let state2 = Eventual.runEventual op2 state1
	putStrLn "After running op2:"
	print $ Eventual.storageNow state2
```

This program outputs

	After running op1:
	fromList []
	After running op2:
	fromList [("foo",4),("bar",16)]
