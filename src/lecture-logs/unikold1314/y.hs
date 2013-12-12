-- The regular (recursive) factorial function
f :: Int -> Int
f = \n -> if n==0 then 1 else n * (f (n-1))

-- An "open" version on the factorial function
g :: (Int -> Int) -> (Int -> Int)
g = \h -> \n -> if n==0 then 1 else n * (h (n-1))

-- The fixed point combinator
y :: ((a -> a) -> (a -> a)) -> (a -> a)
y f = f (y f)

-- Redefine factorial via fixed point combinator
f' = y g

-- Another variation
f'' = g (y g)
