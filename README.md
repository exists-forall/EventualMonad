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

**The `Eventual` monad lets you perform operations which are allowed to depend on elements of a data structure which have not yet been defined.**  When you request an element which has not yet been defined (such as, in the above case, the type signature of `bar`), the monad automatically "queues up" the code which requested that element, suspending its execution.  When the data does finally become available, all of the suspended operations which depended on it are automatically executed.  When compiling the program above, the operation to type-check `foo` would be initially suspended, and run as soon as `bar`'s type signature was set.

## A simple example

Let's take a look at how this actually works in practice.  Keep in mind that this is a *Haskell* library, and this example assumes a familiarity with the language.  If you don't know Haskell, but this all sounds very useful to you, I recommend that you learn it!  Writing code in Haskell is a great time, and it lets you do all sorts of neat little things like this.  I would not recommend trying to implement `Eventual` in C++ (although in a langauge with coroutines, like Lua, you might be able to do it).

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

Woah!  That was a lot of new stuff.

In Haskell, figuring out the types of everythig is usually the best place to start.  Here, we have two "eventual operations" in our little program: `op1` and `op2`.  The type of an eventual operation contains information about what type of "context" it operates in; in our case, `op1` and `op2` both operate within a `Map.Map String Int`.  Any `Map.Map` can be used as a context for eventual values.  `op1` and `op2` both produce no useful value when they finish executing, so their "monad return type" is `()` (this is analogous to the use of `()` in `IO ()`).  The general form of an eventual operation is

```Haskell
Eventual ContextType ProducedValue
```

Putting this all together, it should make sense that both `op1` and `op2`, which are eventual operations operating within a context which maps strings to integers, and which produce no value, have the type:

```Haskell
Eventual (Map.Map String Int) ()
```

To actually define `op1` and `op2`, we use two important `Eventual` primitives: `Eventual.waitGet` and `Eventual.update`.  `Eventual.waitGet` is a monadic operation which gets an eventual value from the eventual context, which may involve suspending execution until the value is defined.  `Eventual.update` simply sets a value in the context.

To accomodate more complex contexts than `Map.Map`, and to provide a layer of encapsulation and abstraction which allows more robust guarantees to be made about the semantics of `Eventual`, `waitGet` and `update` both take "eventual getter" and "eventual update" values, whose specific types vary between context types.  You don't really have to worry about this unless you're implementing your own eventual contexts, so for now, just keep in mind that `eventualKey` can be used to construct a getter suitable for usage with `waitGet`, and `mapUpdate` can be used to produce an updater suitable for use in `update`.

To actually run these eventual operations, we need a context for them to run inside.  We can create one with `eventualState`.  Our storage context is a `Map.Map`, and we want it to start empty, so we can initialize our state with the following:

```Haskell
let
	state0 :: EventualState (Map.Map String Int) -- included for clarity
	state0 = Eventual.eventualState Map.empty
```

Importantly, an `EventualState` keeps track of not just the storage context in which eventual operations can occur, but also the queued operations which are currently waiting for new data.  `Eventual` operations are performed using the function `runEventual`, which is similar to `runState` and friends from other monads.  `runEventual` applies an `Eventual` operation to an `EventualState`, producing a new context.  Its type is:

```Haskell
runEventual :: Eventual w () -> EventualState w -> EventualState w
```

To demonstrate how operations can be suspended when they request undefined data, we start by running `op1`, which depends on the undefined value for key "foo":

```Haskell
let state1 = Eventual.runEventual op1 state0
```

At this point, no values have been set in `state1`'s storage context; the only `update` operations which has been "run" actually depends on the value associated with `"foo"`, and is therefore queued.  To trigger it, we provide a definition for `"foo"`:

```Haskell
let state2 = Eventual.runEventual op2 state1
```

Not only does this set `"foo"` to `100` in the new state, it also triggers the execution of `op1`, which was previously suspended.  `op1` is then free to continue execution to the end, at which point it `update`s the value associated with `"bar"`
