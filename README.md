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

These terms are in weak head normal form:

``` {.haskell .ignore}
T (16 * 16)

\x -> x + 1  -- Lambdas are values too!

7
```

These terms are not:

``` {.haskell .ignore}
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

## Strict fields

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

## Lazy calling convention

## Substitution

## Deriving

## Avoiding surprises

## Lifted types and bottoms on the heap

## Unlifted types
