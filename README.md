---
title: Strict Haskell
author: Thomas Tuegel
...

# Strict Haskell

## Goals

The goals of this talk are:

- to _educate_ about the tools available for controlling evaluation and measuring performance
- to _report_ my own experience managing performance of a large Haskell application
- to _convince_ that writing a strict dialect of Haskell is a reasonable choice
  for anyone with a performance-sensitive Haskell application.

# Evaluation

## Pattern matching

Evaluation is driven by pattern matching.

We can use `trace` or `undefined` to explore how this happens.

<!-- If you liked Minesweeper, you'll love debugging with `undefined`! -->

``` {.haskell .lazy}
import Debug.Trace (trace)

outside, inside, rhs, irrelevant, body :: a -> a
outside = trace "outside"
inside = trace "inside"
rhs = trace "rhs"
irrelevant = trace "irrelevant"
body = trace "body"

data T = T Integer
  deriving Show

newtype N = N Integer
  deriving Show
```

``` {.haskell .lazy}
example_HVgx =
    case outside (T (inside (1 + 1))) of
        T _ -> ()
```

Note: `trace` is polymorphic, so it doesn't care so much where you put the parentheses.
That can affect when it fires, though!

## Pattern matching

For each pattern, the expression is evaluated until it can be matched or refuted:

``` {.haskell .lazy}
example_MGPj =
    case outside (T (inside (1 + 1))) of
        T 3 -> () -- Forces the argument of T, even though it doesn't match.
        T _ -> ()
```

The first pattern forces the complete evaluation even though it does not match:
we have to evaluate the argument of `T` to decide if the pattern matches.

There is no magic! The runtime system evaluates terms as needed to match patterns,
nothing more and nothing less.

## Irrefutable patterns

An _irrefutable pattern_ defers evaluation until the bound variables are demanded:

``` {.haskell .lazy}
-- refutable
example_zePe =
    case outside (T (inside (1 + 1))) of
        T a -> rhs (a + 1)

-- irrefutable
example_xCHk =
    case outside (T (inside (1 + 1))) of
        ~(T a) -> rhs (a + 1)
```

As the name implies, however, an irrefutable pattern always matches, so we must take care with sum types:

``` {.haskell .lazy}
example_GuXI =
    case Nothing :: Maybe Integer of
        ~(Just a) -> a + 1
```

## Sharing

A named expression will be evaluated at most once, that is: the result of
evaluating an expression is _shared_.

``` {.haskell .lazy}
shared :: T
shared = trace "shared" (T 4)

example_CZbP =
    case shared of
        T _ -> ()

example_QTdb =
    case shared of
        T 3 -> ()
        T _ -> ()
```

## Weak head normal form

We can evaluate any expression by pattern matching, if the constructors of its
type are in scope.

If a term is evaluated to its outermost constructor or lambda, we say it is in
_weak head normal form_.

Note: `newtype` constructors are _not_ real constructors!

These terms are in weak head normal form:

``` {.haskell .ignore}
T (16 * 16)

N 16

\x -> x + 1  -- Lambdas are values too!

7
```

These terms are not:

``` {.haskell .ignore}
N (4 * 4)

if (8 * 8) == 64 then A 1 else A 0

(\x -> x + 1) . (\x -> 3 * x)

3 + 4
```

## `seq`

The compiler gives us a magic tool to evaluate expressions to weak head normal
form even if we don't know the type's constructors!

``` {.haskell .lazy}
strictly :: T
strictly = seq (irrelevant (8 * 8 :: Integer)) (inside (T (16 * 16)))
```

``` {.haskell .lazy}
example_Xplw =
    case strictly of
        T _ -> ()
```

`irrelevant` is evaluated because we have to pass through `seq` to get to the
outermost constructor.

## `BangPatterns`

With the `BangPatterns` extension enabled, use `!` before any pattern to
evaluate it to weak head normal form before matching:

``` {.haskell .lazy}
example_upsS =
    case T (inside (1 + 1)) of
        T !x -> const () x
```

We can use `!` anywhere in the pattern, but it may have no added effect:

``` {.haskell .lazy}
example_CUoA =
    case T (inside (1 + 1)) of
        !(T x) -> const () x  -- legal, but not very useful
```

This is equivalent to using `seq`:

``` {.haskell .lazy}
example_GtKP =
    case T (inside (1 + 1)) of
        T x -> seq x (const () x)
```

`BangPatterns` tends to be used more in practice than `seq` itself.

## `BangPatterns` in `let` and `where`

The order of evaluation in `let` and `where` blocks is subtle.

Top-level strict bindings are evaluated before the body of the block:

``` {.haskell .lazy}
example_ptVB =
    let !x = outside 1
    in body (T x)
```

Nested strict bindings are evaluated when a variable bound in the same pattern
is demanded:

``` {.haskell .lazy}
example_MESN =
    let (x, !y) = (1, inside 2)
    in body (T x, T y)
```

## `newtype` and `BangPatterns`

`seq` and `!` may have unexpected consequences with `newtype`s:

``` {.haskell .lazy}
example_Oaya =
    case N (inside (4 * 4)) of
        !(N x) -> const () x
```

Remember, `newtype` constructors are not real constructors! Evaluating a
`newtype` to weak head normal form is the same as evaluating its argument.

## Strict fields

Besides binding patterns strictly, we can make constructor fields strict with `!`.
This is not an extension, but part of the Haskell 2010 Language Report:

``` {.haskell .lazy}
data S = S !Integer
  deriving Show
```

``` {.haskell .lazy}
example_Zdji =
    case S (inside (16 * 16)) of
        S _ -> ()

example_UlWi =
    case T (inside (16 * 16)) of
        T _ -> ()
```

There is no way to lazily bind a strict field, except to bind the entire
constructor with an irrefutable pattern:

``` {.haskell .lazy}
example_emjP =
    case S (inside (16 * 16)) of
        ~(S _) -> ()
```

## `StrictData`

The `StrictData` extension makes all constructor fields strict by default,
for data types defined in a module where it is enabled, as if they were prefixed with `!`.

``` {.haskell .ignore}
-- These are the same, with StrictData enabled.

data S = S !Integer

data S' = S' Integer
```

To make a field lazy, prefix its type with `~`:

``` {.haskell .ignore}
data T' = T ~Integer
```

`StrictData` is a popular extension that is often regarded as at least harmless,
but we should remain aware of the consequences of making fields strict!

## `Strict`

The `Strict` extension makes pattern bindings strict by default, and also enables `StrictData`.

`Strict` applies a `!`-pattern at the top level of patterns:

``` {.haskell .ignore}
-- function definitions
f !x = _

-- lambdas
\ !x -> _

-- case expressions
case y of
    !x -> _

-- let and where bindings
let !x = y
in _

-- do notation
do
    !x <- y
    _

-- list comprehensions
[ _ | !y <- x ]
```

## `Strict`

`Strict` does _not_ apply `!` to nested patterns:

``` {.haskell .ignore}
case t of
    !(x, y) -> _  -- ! at top level, but not on x and y
```

or at the top-level of a module:

``` {.haskell .ignore}
module M where

-- No !-pattern at all
(x, y) = _
```

All the usual caveats about `BangPatterns` and `StrictData` apply.

# Debugging

## Thunks and space leaks

A _thunk_ is what we call (the runtime representation of) an expression that is
not in weak head normal form.

As we have seen, constructors may be lazy, so that expressions in weak head normal form
may contain thunks themselves.

<!-- It's thunks all the way down. -->

Thunks can be a problem if the thunk uses more space than the value it represents.
For example, consider the thunk `1 + 1` to the value `2`.

<!-- Every non-trivial Haskell program leaks thunks. -->

## Heap profiling

Here is a simple program that leaks space in a loop:

``` {.haskell .sawtooth}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.IORef

main :: IO ()
main = do
    r <- newIORef 0
    loop r 0

increment :: IORef Integer -> IO ()
increment r = modifyIORef r (\x -> x + 1)

loop :: IORef Integer -> Int -> IO ()
loop r n
    | n > 1_000_000 = pure ()
    | otherwise = do
        when (n `mod` 100_000 == 0) $ do
            x <- readIORef r
            print x  -- serialization barrier
        increment r
        threadDelay 100  -- delay to make heap graph more readable
        loop r (n + 1)
```

## Interpreting heap profiles

## Limitations of heap profiling

- Heap profiling is a _sampled_ profiling mode. Any features smaller than the
  sampling interval (default: 0.1 seconds) cannot reliably be detected.

<!-- We can increase the sampling frequency, but gathering more data is slower to run and more difficult to analyze. We don't have a way to increase the frequency selectively. -->

<!-- Do you work on a web app backend? Is your request-response cycle faster than 200 ms? By Nyquist-Shannon theorem, you can't reliably detect a thunk leak with the default settings. You can increase the frequency, but it has a cost. -->

- Heap profiling loses context.

<!-- We cannot directly identify the root cause of allocation from the heap profile because only the top of the cost centre stack is shown. -->

## Cost centre profiling - Laziness

GHC User's Guide, Section 8.1.2: (Emphasis mine.)

> While running a program with profiling turned on, GHC maintains a cost-centre
> stack behind the scenes, and attributes any costs (memory allocation and time)
> to whatever the current cost-centre stack is at the time the cost is incurred.

> The mechanism is simple: whenever the program evaluates an expression with an
> SCC annotation, `{-# SCC c -#} E`, the cost centre `c` is pushed on the current
> stack, and the entry count for this stack is incremented by one. The stack
> also sometimes has to be saved and restored; in particular when the program
> creates a thunk (a lazy suspension), the current cost-centre stack is stored
> in the thunk, and restored when the thunk is evaluated. In this way, the
> cost-centre stack is _independent of the actual evaluation order_ used by GHC at
> runtime.

The cost center profile cannot show where a thunk is forced because the
cost centre stack is independent of the evaluation order.

## Cost centre profiling - Laziness

``` {.haskell .small}
module Main (main) where

import Control.Monad (unless)

data Lazy = Lazy Integer
    deriving (Show)

square :: Integer -> Integer
square x = x * x

double :: Integer -> Integer
double x = x + x

small :: Integer -> IO Lazy
small x = do
    unless (x > 2) $ putStrLn "x is small"
    pure $ Lazy (square x)

main :: IO ()
main = do
    r <- small (double 2)
    print r
```

## Cost centre profiling - Laziness

```
                                                           individual      inherited
COST CENTRE  SRC                         no.     entries  %time %alloc   %time %alloc

MAIN         <built-in>                  121           0    0.0    1.3     0.0  100.0
 CAF         <entire-module>             241           0    0.0    0.1     0.0    1.0
  main       README.lhs:(437,1)-(439,11) 242           1    0.0    0.2     0.0    0.9
   double    README.lhs:429:1-16         246           1    0.0    0.0     0.0    0.0
   small     README.lhs:(432,1)-(434,26) 244           1    0.0    0.7     0.0    0.7
    square   README.lhs:426:1-16         248           1    0.0    0.0     0.0    0.0
```

<!-- I omitted the MODULE column to save space. -->

- Where is `double` evaluated?
- Where is `square` evaluated?

We can answer these questions, but not by using the cost centre profile.

## Limitations of cost centre profiling

## How `Strict` helps

# Applications and caveats

## Switching to Strict

## Lazy calling convention

## Substitution

## Deriving

## Avoiding surprises

## Lifted types and bottoms on the heap

## Unlifted types

## Testing and undefined
