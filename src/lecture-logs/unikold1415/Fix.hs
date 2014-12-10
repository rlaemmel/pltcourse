{-

(C) 2014 Ralf Laemmel

Let's play with fixed points.

A fixed point of f is an x such that f x == x

-}

-- The definitional property as function combinator
fix f = f (fix f)

{-

We will now model a recursive function is a fixed point of a
non-recursive function.

-}

-- The recursive function for comparison
fac = \x -> if x == 0 then 1 else x * fac (x - 1)

-- Factor out recursion as an extra argument
fac' = \f x -> if x == 0 then 1 else x * f (x - 1)

-- Reobtaining the factorial function
fac'' = fix fac'

{-

*Main> fac 5
120
*Main> fac' undefined 5
*** Exception: Prelude.undefined
*Main> fac' undefined 0
1
*Main> fac' fac 5
120
*Main> fac' undefined 1
*** Exception: Prelude.undefined
*Main> fac' (fac' undefined) 1
1

-}

{-

The bottom line is that explicitly recursive functions are not needed
for as long as we can take the fixed points of functions. The
corresponding fix combinator happens to be a recursive function. In
Haskell, thus, we need at least that single recursive function fix.

-}

{-

Another example:

*Main> fix (\f x -> if x==0 then True else not (f (x-1))) 0
True
*Main> fix (\f x -> if x==0 then True else not (f (x-1))) 1
False
*Main> fix (\f x -> if x==0 then True else not (f (x-1))) 2
True

-}