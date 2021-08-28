---
title: Strict Haskell
author: Thomas Tuegel
...

# Strict Haskell

# Evaluation

## Pattern matching

Evaluation is driven by pattern matching.

We can use `trace` or `undefined` to explore how this happens.

<!-- If you liked Minesweeper, you'll love debugging with `undefined`! -->

``` {.haskell .section-01}
data T a = T a

newtype N a = N a

lazy :: T Integer
lazy = trace "outside" (A (trace "inside" (16 * 16)))
```

``` {.haskell .ignore}
case lazy of
    T _ -> ()
```

Note: `trace` is polymorphic, so it doesn't care so much where you put the parentheses.
That can affect when it fires, though!

`lazy` is _shared_ so that subsequent matches use the same value already computed.

## Pattern matching

For each pattern, `lazy` is evaluated until it can be matched or refuted:

``` {.haskell .ignore}
case lazy of
    T 128 -> () -- Forces the argument of T, even though it doesn't match.
    T _ -> ()
```

The first pattern forces the complete evaluation of `lazy` even though it does not match:
we have to evaluate the argument of `T` to decide if the pattern matches.

There is no magic! The runtime system evaluates terms as needed to match patterns,
nothing more and nothing less.

## Irrefutable patterns

An _irrefutable pattern_ defers evaluation until the bound variables are demanded:

``` {.haskell .ignore}
case lazy of
    ~(T a) -> trace "right-hand side" (a + 1)
```

As the name implies, however, an irrefutable pattern always matches, so we must take care with sum types:

``` {.haskell .ignore}
case Nothing :: Maybe Integer of
    ~(Just a) -> a + 1
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

``` {.haskell .section-01}
strictly :: T Integer
strictly = trace "outside" (seq irrelevant (trace "inside" (T (16 * 16))))
  where
    irrelevant = trace "irrelevant" (8 * 8)
```

``` {.haskell .ignore}
case lazy of
    A _ -> 8
```

`irrelevant` is evaluated because we have to pass through `seq` to get to the outermost constructor.

``` {.haskell .ignore}
trace "outside" (seq irrelevant (trace "inside" (T _)))
    -- print "outside"
seq irrelevant (trace "inside" (T _))
    -- evaluate 'irrelevant'
trace "inside" (T _)
    -- print "strictly" and "inside" (no order guarantee!)
T _
```

## `BangPatterns`

`BangPatterns` is a language extension to control strictness on a pattern-by-pattern basis. We can use `!` before any pattern and it will be evaluated to weak head normal form before matching:

``` {.haskell .ignore}
case lazy of
    T !x -> const () x
```

We can use `!` anywhere in the pattern, but it may have no added effect:

``` {.haskell .ignore}
case lazy of
    !(T x) -> const () x  -- legal, but not very useful
```

This is equivalent to using `seq`:

``` {.haskell .ignore}
case lazy of
    T x -> seq x (const () x)
```

In practice, `BangPatterns` sees more use than `seq` itself.

## `newtype` and `BangPatterns`

`seq` and `!` may have unexpected consequences with `newtype`s:

``` {.haskell .ignore}
case N (trace "inside" 16 :: Integer) of
    !(N x) -> const () x
```

Remember, `newtype` constructors are not real constructors!

## Strict fields

Besides binding patterns strictly, we can make constructor fields strict with `!`.
This is not an extension, but part of the Haskell 2010 Language Report:

``` {.haskell .section-01}
data S a = S !a
```

``` {.haskell .ignore}
case S (inside (16 * 16)) of
    S _ -> ()

case T (inside (16 * 16)) of
    T _ -> ()
```

Unlike `BangPatterns`, this is not equivalent to `seq`.
In particular, there is no way to lazily bind a strict field, except binding the entire constructor with an irrefutable pattern:

``` {.haskell .ignore}
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
