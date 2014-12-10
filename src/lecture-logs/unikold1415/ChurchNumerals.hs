{-

        (C) 2014 Ralf Laemmel

        Illustration of Church numerals in Haskell's lambda calculus

-}

import Prelude hiding (succ)

-- Constants 0, 1, 2, 3
c0 = \s z -> z
c1 = \s z -> s z
c2 = \s z -> s (s z)
c3 = \s z -> s (s (s z))

-- Successor function
succ = \n s z -> s (n s z)

-- Addition defined by primitive recursion
plus = \m n s z -> m s (n s z)

-- Multiplication defined by primitive recursion
times = \m n -> m (plus n) c0

-- Converting Church numerals into regular natural numbers
showChurch n = n (+1) 0

{-

*Main> showChurch c0
0
*Main> showChurch c1
1
*Main> showChurch c2
2
*Main> showChurch c3
3
*Main> showChurch (succ c3)
4
*Main> showChurch (plus c3 c2)
5
*Main> showChurch (times c3 c2)
6

-}
