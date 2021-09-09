---
title: Strict Haskell
author: Thomas Tuegel
patat:
  images:
    backend: kitty
...

# Strict Haskell

## Goals

You will learn:

- How evaluation happens, and how to control it
    - Pattern matching
    - `seq`
    - Strict patterns
    - Strict fields
- How to measure performance
    - Cost centre profiling
    - Heap profiling
- Why you should and should not use Strict Haskell

# Evaluation

## Tracing evaluation

`trace` prints a message before its second argument is evaluated.
Use it to explore when expressions are evaluated.

``` {.haskell .lazy}
import Debug.Trace (trace)

outside, inside, rhs, irrelevant, body :: a -> a
outside = trace "outside"
inside = trace "inside"
rhs = trace "rhs"
irrelevant = trace "irrelevant"
body = trace "body"
```

Example:

``` {.haskell .lazy}
data T = T Integer
  deriving Show

newtype N = N Integer
  deriving Show

example_bCYB =
    outside (T (inside 4))
```

## Pattern matching

The essense of evaluation:
At each pattern, the expression is evaluated until it does or does not match.

``` {.haskell .lazy}
example_HVgx =
    case outside (T (inside (1 + 1))) of
        T _ -> ()
```

Step-by-step:

``` {.haskell .ignore}
outside (T (inside (1 + 1)))  -- matches (T _)? unknown
-- eval: outside
T (inside (1 + 1))  -- matches (T _): yes
-- done
```

## Pattern matching

The essense of evaluation:
At each pattern, the expression is evaluated until it does or does not match.

``` {.haskell .lazy}
example_MGPj =
    case outside (T (inside (1 + 1))) of
        T 3 -> () -- Forces 'inside', even though it doesn't match.
        T _ -> ()
```

Step-by-step:

``` {.haskell .ignore}
outside (T (inside (1 + 1)))  -- matches (T 3)? unknown
-- eval: outside
T (inside (1 + 1))  -- matches (T 3)? unknown
-- eval: inside
T (1 + 1)  -- matches (T 3)? unknown
T 2  -- matches (T 3)? no
-- done
```

Every other technique for controlling evaluation builds on this.

## Irrefutable patterns

An _irrefutable pattern_ defers evaluation until the bound variables are
demanded:

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

Irrefutable patterns always match. Take care with sum types!

``` {.haskell .lazy}
example_GuXI =
    case Nothing :: Maybe Integer of
        ~(Just a) -> a + 1
```

## Sharing

An expression will be evaluated at most once, that is:
the result of evaluation is _shared_.

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

## `seq`

If we know the constructors, we can evaluate any expression by pattern
matching. What if the constructors are not in scope, or the type is not
known?

`seq` evaluates its first argument and returns the second:

``` {.haskell .lazy}
strictly :: T
strictly = seq (irrelevant (8 * 8 :: Integer)) (inside (T (16 * 16)))
```

``` {.haskell .lazy}
example_Xplw =
    case strictly of
        T _ -> ()
```

`irrelevant` is evaluated because we have to pass through `seq` to get to
the outermost constructor.

## Weak head normal form

`seq` evaluates its first argument to weak head normal form.

A term is in _weak head normal form_ if it is evaluated to its outermost
constructor or lambda.

Examples:

``` {.haskell .ignore}
T (16 * 16)

\x -> x + 1  -- Lambdas are values too!

7
```

Counter-examples:

``` {.haskell .ignore}
N (4 * 4)  -- 'newtype' constructors are not real constructors!

if (8 * 8) == 64 then A 1 else A 0

(\x -> x + 1) . (\x -> 3 * x)

3 + 4
```

## Thunks

A term that is not in weak head normal form is called a _thunk_.

A thunk may be bottom (`_|_`), but a term in weak head normal form is
known not to be bottom.

> The value of `seq a b` is bottom if `a` is bottom,
> and otherwise equal to `b`.

From here on, "evaluate" shall mean "evaluate to weak head normal form"
unless otherwise specified.

## `BangPatterns`

Status: extension since GHC 6.8.1

Use `!` before a pattern to evaluate it before matching:

``` {.haskell .lazy}
example_upsS =
    case T (inside (1 + 1)) of
        T !x -> const () x
```

This is equivalent to inserting `seq`:

``` {.haskell .lazy}
example_GtKP =
    case T (inside (1 + 1)) of
        T x -> seq x (const () x)
```

`BangPatterns` is a little more convenient than inserting `seq` by hand,
and it tends to be used more in practice.

## `BangPatterns`

We can use `!` anywhere in the pattern, but it may have no added effect:

``` {.haskell .lazy}
example_CUoA =
    case T (inside (1 + 1)) of
        !(T x) -> const () x  -- legal, but not very useful
```

The pattern `T x` already evaluates the expression to weak head normal form.

## `BangPatterns` in `let` and `where`

The order of evaluation in `let` and `where` blocks is subtle.

`!`s at the top of patterns are evaluated before the body of the block:

``` {.haskell .lazy}
example_ptVB =
    let !x = outside 1
    in body (T x)
```

Nested `!`s are evaluated when a variable bound in the same pattern is used:

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

Status: Haskell 2010

Make a field strict by prefixing its type with `!`:

``` {.haskell .lazy}
data S = S !Integer
  deriving Show
```

Strict fields are evaluated when the constructor is evaluated:

``` {.haskell .lazy}
example_Zdji =  -- strict
    case S (inside (16 * 16)) of
        S _ -> ()

example_UlWi =  -- lazy
    case T (inside (16 * 16)) of
        T _ -> ()
```

## Strict fields

There is no way to lazily bind a strict field, except to bind the entire
constructor with an irrefutable pattern:

``` {.haskell .lazy}
example_emjP =
    case S (inside (16 * 16)) of
        ~(S _) -> ()
```

Think carefully when you add a strict field to a sum type!

## `StrictData`

Status: extension since GHC 8.0.1

Make all constructor fields strict by default.

These are the same when `StrictData` is enabled:

``` {.haskell .ignore}
data S = S !Integer

data S' = S' Integer
```

To make a field lazy, prefix its type with `~`:

``` {.haskell .ignore}
data T' = T ~Integer
```

`StrictData` is a popular extension, but we should remain aware of the
consequences of using strict fields.

## `Strict`

Status: extension since GHC 8.0.1. Implies `StrictData`.

Make patterns strict by default by applying `!` implicitly at the top:

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

# Profiling

## Why measure?

Every thunk is a trade-off:

- we spend less time evaluating, but
- we use more memory to hold the thunk (often).

We have to control how we make this trade-off because there isn't a
single solution that works for all cases.

We cannot control what we cannot measure.

Every non-trivial Haskell program leaks thunks.

## Cost centre profiling

Cost centre profiling reports

- time spent
- memory allocated

by sections of code ("cost centres").

Build with profiling:
`ghc -prof -fprof-auto-top -rtsopts ...`

`-prof`: enable profiling
`-fprof-auto-top`: assign a cost centre to every top-level definition
`-rtsopts`: enable setting RTS options at runtime (potentially insecure)

Run with profiling:
`./main ... +RTS -p`

`+RTS`: send options to the RTS
`-p`: enable cost centre profiling

## Cost centre profiling - Example

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

## Cost centre profiling - Example

```
                    individual      inherited
COST CENTRE       %time %alloc   %time %alloc

MAIN                0.0    1.3     0.0  100.0
 CAF                0.0    0.1     0.0    1.0
  main              0.0    0.2     0.0    0.9
   double           0.0    0.0     0.0    0.0
   small            0.0    0.7     0.0    0.7
    square          0.0    0.0     0.0    0.0
```

<!-- I omitted some columns to save space. -->

- Where is `double` evaluated?
- Where is `square` evaluated?

We can answer these questions, but not by using the cost centre profile.

## Cost centre profiling - Laziness

GHC User's Guide, Section 8.1.2: (Emphasis mine.)

> While running a program with profiling turned on, GHC maintains a
> cost-centre stack behind the scenes, and attributes any costs (memory
> allocation and time) to whatever the current cost-centre stack is at the
> time the cost is incurred.

> The mechanism is simple: whenever the program evaluates an expression
> with an SCC annotation, `{-# SCC c -#} E`, the cost centre `c` is pushed
> on the current stack, and the entry count for this stack is incremented
> by one. The stack also sometimes has to be saved and restored; in
> particular when the program creates a thunk (a lazy suspension), the
> current cost-centre stack is stored in the thunk, and restored when the
> thunk is evaluated. In this way, the cost-centre stack is _independent
> of the actual evaluation order_ used by GHC at runtime.

The cost center profile cannot show where a thunk is forced because the
cost centre stack is independent of the evaluation order.

## Cost centre profiling - Recursion

GHC User's Guide, Section 8.1: (Emphasis mine.)

> What about recursive functions, and mutually recursive groups of
> functions? Where are the costs attributed? Well, although GHC does keep
> information about which groups of functions called each other
> recursively, this information isn’t displayed in the basic time and
> allocation profile, instead the call-graph is flattened into a tree as
> follows: _a call to a function that occurs elsewhere on the current
> stack does not push another entry on the stack, instead the costs for
> this call are aggregated into the caller_.

The cost centre profile will appear to mis-attribute costs in the case of
recursive functions. Of course, the costs aren't _really_ mis-attributed;
it's just not possible to distinguish the call sites.

<!-- This isn't directly related to Strict and profiling with laziness, it's just something everybody should know. -->

## Heap profiling

Heap profiling reports the contents of the heap, broken down by the cost
centre that allocated them.

Run with heap profiling:
`./main ... +RTS -h -i0.01`

`-h`: enable heap profiling
`-i0.01`: set the heap sampling interval to 0.01 seconds

## Heap profiling - Example

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
        when (n `mod` 100_000 == 0)
            (readIORef r >>= print)  -- serialization barrier
        increment r
        threadDelay 100  -- delay to make heap graph more readable
        loop r (n + 1)
```

## Heap profiling - Example

![](sawtooth.png)

## Heap profiling - Limitations

-   Walking the heap is expensive, so the default sample interval is long.

    Thought experiment:

    The default sample interval is 0.1 s = 100 ms.
    Nyquist-Shannon theorem: you can't reliably measure under 200 ms.
    If your application's response time is less than 200 ms,
    then you wouldn't know if you have space leaks or not.

<!-- We can increase the sampling frequency, but gathering more data is slower to run and more difficult to analyze. We don't have a way to increase the frequency selectively. -->

-   Heap profiling loses context.

    Cost centre profiling shows a nice tree of cost centres,
    but heap profiling can't show the relation between them so easily.

    It's hard to find the _root_ cause of leaks.

# Why Strict? Why NOT Strict?

## Experience report

My team struggled for months debugging space leaks in a 120+ kLoC Haskell
application, and even then felt that we had only scratched the surface.

We made the switch to Strict Haskell. Within a week, we identified and
fixed more performance bugs than we had in the preceeding months, and we
finally felt confident that we had control of the substantial performance
concerns.

<!-- This is essentially my entire argument for `Strict`: it made my team happier and more productive. -->

There are two ways that `Strict` helped us control performance:

1. `Strict` tends to turn thunk (space) leaks into wasted time.

    ```
    default = lazy      =>          delay evaluation    =>   leak space

    default = strict    =>    unnecessary evaluation    =>   waste time
    ```

    Debugging programs that _waste time_ is easier than debugging programs
    that _leak space_.

    <!-- This is not a cure-all! Strict will not make your program faster. We are trading one type of problem for another. -->

2. `Strict` tends to make the evaluation order match the cost centre profile.

    Debugging programs that _waste time_ is easier when the evaluation
    order matches the cost centre profile.

    <!-- What about intentional laziness? With Strict, it's easier to spot intentional laziness in the code. -->

## Switching to Strict

1.  Collect end-to-end performance measurements.

    This doesn't need to be complicated: the `time` command is good enough
    to identify a problem exists.

    <!-- A problem looks like: a spike in time or space use. -->

    You already have end-to-end benchmarks, right?

    You at least have end-to-end _tests_, right?

    Problems arise from complex interactions between components.
    Microbenchmarks are not very useful.

2.  Turn on `Strict` in one module. Run the benchmarks. Repeat.

    Problems are unfortunately still non-local.

    This part is tedious in a large project...
    Perhaps a reason to start with `Strict` in the first place!

3.  Finally, add `Strict` to `default-extensions`.

## Lazy data types

Problem: we find a type that is expensive to compute, but rarely demanded.
It should be bound lazily by default, but it's hard to remember to use `~`
everywhere. What do we do?

Introduce a lazy wrapper. The most general case:

``` {.haskell .ignore}
data Lazy a = Lazy ~a
```

## Deriving

`DeriveFunctor` behaves badly in with `Strict`.

<!-- Actually, it behaves badly any time strict fields are involved. -->

Example:

``` {.haskell .functor}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveFunctor #-}

module StrictDeriving where

newtype Id1 a = Id1 a
    deriving (Functor, Show)

-- Composition law:

-- >>> fmap (const 2 . const undefined) (Id1 1)
-- Id1 2

-- >>> (fmap (const 2) . fmap (const undefined)) (Id1 1)
-- undefined
```

Workarounds:

1.  Ignore it. Could be an unpleasant surprise for developers in an
    open-source library.
2.  Use `NoStrict` in the module where `Functor` is derived. Mark fields lazy.
3.  Avoid `DeriveFunctor`.

## Do we care about bottom?

A common complaint about `Strict` is that it breaks substitution with bottom:

``` {.haskell .ignore}
-- With Strict, this:
let x = f y in g x
-- is only the same as this:
g (f y)
-- if (f y) is not bottom or (g) is strict.
```

Do we care? Empirically: no.

1.  We usually do equational reasoning under an implicit assumption that
    all values are defined; that is, we don't think about bottoms at all.

2.  If we cared how our code handled bottoms, we would write tests for that.

## Why don't we care about bottom?

>     "Fast and Loose Reasoning is Morally Correct" (doi:10.1.1.63.1337)
>         Nils Anders, Danielsson John Hughes, Patrik Jansson
>
> Functional programmers often reason about programs as if they were
> written in a total language, expecting the results to carry over to
> non-total (partial) languages. We justify such reasoning. ... It is
> proved that if two closed terms have the same semantics in the total
> language, then they have related semantics in the partial language.

We mostly work in domains with models where bottom doesn't mean anything.

<!-- For example, bank account details, social media profile, etc. -->

## Unlifted types

With `Strict`, we are still being a little imprecise with bottoms,
but in the future we may be able to be totally precise.

Normal data types are _lifted_ meaning they allow bottom,
as opposed to certain primitive types which are _unlifted_.

Proposal to add user-defined _unlifted_ data types to GHC:
https://gitlab.haskell.org/ghc/ghc/-/wikis/unlifted-data-types

This talk in a few years, probably:

> If bottom has a meaning in your domain model, use lifted data.
> For everything else, use unlifted data.

## Recommendations

-   Measure performance now.

    Everyone says they don't care about performance, until it's _too slow_.

-   Measure more.

    Your Haskell program leaks memory in ways you aren't testing for.

-   Use `Strict` to easily debug the problems you measure.

<!-- I've equipped you to do the first two even if you aren't prepared to take the leap. -->
