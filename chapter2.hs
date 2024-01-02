--fibonacci lazy list

fibs = 0 : 1 : helper fibs (tail fibs)
    where
        helper (a:as)(b:bs) =
            a + b : helper as bs


--reversing a list with foldl
reversel :: [a] -> [a]
reversel list = foldl (\acc x -> x:acc) [] list

--reversing a list with foldr
reverser :: [a] -> [a]
reverser list = foldr (\x acc -> acc++[x]) [] list

--implementing zipWith using list comprehension
zipWithLC :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithLC func list1 list2 = [func num1 num2 | (num1, num2) <- zip list1 list2]

--implementing zipWith without list comprehension
zipWithNoLC :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithNoLC _ [] _ = []
zipWithNoLC _ _ [] = []
zipWithNoLC func list1 list2 = (func (head list1) (head list2)):(zipWithNoLC func (tail list1) (tail list2))

--implementing zipWith with foldl
zipWithFold :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithFold func list1 list2 = foldl (\acc x -> acc ++ [func (fst x) (snd x)]) [] (zip list1 list2)

--implementing ConcatMap with foldl
concatMapFoldl :: ([a] -> [b]) -> [[a]] -> [b]
concatMapFoldl func list = foldl (\acc x -> acc ++ x) [] (foldl (\acc x -> acc ++ [func x]) [] list)