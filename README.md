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

outside, inside, rhs, irrelevant :: a -> a
outside = trace "outside"
inside = trace "inside"
rhs = trace "rhs"
irrelevant = trace "irrelevant"

data T = T Integer

newtype N = N Integer
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

A named expression will be evaluated at most once, that is:
the result of evaluating an expression is _shared_.

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

We can evaluate any expression by pattern matching, if the constructors of its type are in scope.

If a term is evaluated to its outermost constructor or lambda, we say it is in _weak head normal form_.

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

The compiler gives us a magic tool to evaluate expressions to weak head normal form even if we don't know the type's constructors!

``` {.haskell .lazy}
strictly :: T
strictly = seq (irrelevant (8 * 8 :: Integer)) (inside (T (16 * 16)))
```

``` {.haskell .lazy}
example_Xplw =
    case strictly of
        T _ -> ()
```

`irrelevant` is evaluated because we have to pass through `seq` to get to the outermost constructor.

## `BangPatterns`

`BangPatterns` is a language extension to control strictness on a pattern-by-pattern basis. We can use `!` before any pattern and it will be evaluated to weak head normal form before matching:

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

In practice, `BangPatterns` sees more use than `seq` itself.

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
```

``` {.haskell .lazy}
example_Zdji =
    case S (inside (16 * 16)) of
        S _ -> ()

example_UlWi =
    case T (inside (16 * 16)) of
        T _ -> ()
```

Unlike `BangPatterns`, this is not equivalent to `seq`.
In particular, there is no way to lazily bind a strict field, except binding the entire constructor with an irrefutable pattern:

``` {.haskell .lazy}
example_emjP =
    case S (inside (16 * 16)) of
        ~(S _) -> ()
```

Strict fields are widely used and very useful, but we should take care because they come with limitations.

## `StrictData`

## `Strict`

# Debugging

## Thunks and space leaks

## Heap profiling

## Interpreting heap profiles

## Limitations of heap profiling

## Cost centre profiling

## Interpreting cost centre profiles

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
