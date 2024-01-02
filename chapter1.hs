---Factorials

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * (factorial (n-1))

---Fibonacci

fibonacci :: Int -> Int
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fibonacci (n-1)) + (fibonacci (n-2))

--Currying and uncurrying

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x, y) = f x y

curry' :: ((a,b)->c) -> a -> b -> c
curry' f x y = f (x, y)

