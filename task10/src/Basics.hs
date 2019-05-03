module Basics where

-- Цель первой части домашнего задания -- познакомить вас с основами синтаксиса Хаскеля
-- В этом задании запрещается использовать какие-либо функции из стандартной библиотеки.
-- Однако разрешается использовать реализованные самостоятельно

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = (+1) (length' xs)

-- 1. head' возвращает первый элемент непустого списка
head' :: [a] -> a
head' [] = error "There is no head, because list is empty!"
head' (x:_) = x

-- 2. tail' возвращает список без первого элемента, для пустого - пустой
tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

-- 3. take' возвращает первые n >= 0 элементов исходного списка
take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- 4. drop' возвращает список без первых n >= 0 элементов; если n больше длины
-- списка, то пустой список.
drop' :: Int -> [a] -> [a]
drop' n xs
  | n <= 0 = xs
  | n >= len = []
  where len = length' xs
drop' n (_:xs) = drop' (n-1) xs

-- 5. filter' возвращает список из элементов, для которых f возвращает True
filter' :: (a -> Bool) -> [a] -> [a]
filter' f (x:xs) 
  | f x = x : xs'
  | otherwise = xs'
  where xs' = filter' f xs
filter' _ [] = []

-- 6. foldl'' последовательно применяет функцию f к элементу списка l и значению,
-- полученному на предыдущем шаге, начальное значение
-- foldl'' (+) 0 [1, 2, 3] == (((0 + 1) + 2) + 3)
-- foldl'' (*) 4 [] == 4
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ z [] = z
foldl'' f z (x:xs) = foldl'' f (f z x) xs 

-- 7. concat' принимает на вход два списка и возвращает их конкатенацию
-- concat' [1,2] [3] == [1,2,3]
concat' :: [a] -> [a] -> [a]
concat' x y = x ++ y 

-- 8. quickSort' возвращает его отсортированный список
-- quickSort' должен быть реализован через алгоритм QuickSort
-- (выбор pivot может быть любым)
quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = 
  let smallerElems = quickSort' [a | a <- xs, a <= x]
      largerElems = quickSort' [a | a <- xs, a > x]
  in smallerElems ++ [x] ++ largerElems
