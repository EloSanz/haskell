module Listas  where

mi_max :: Ord a => a -> a -> a
mi_max a b = if a > b then a else b

iguales :: Int -> Int -> Bool
iguales a b = a == b

sumarLista :: Num a => [a]->a
sumarLista [] = 0
sumarLista(x:xs) = x + sumarLista xs

contarLista :: Num a => [a] -> a
contarLista [] = 0
contarLista(_:xs) = 1 + contarLista xs

search :: Eq a => a -> [a] -> Int
search x xs = _search x xs 0

_search :: Eq a => a -> [a] -> Int -> Int
_search _ [] _ = -1
_search k (x:xs) q = if k == x then q else _search k xs (q + 1)


reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = xs ++ [x]


concatenar :: [Int] -> [Int]
concatenar xs = xs ++ [3..5]

eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = x : eliminarDuplicados( filter( /= x) xs ) 

duplicar :: Num a => [a] -> [a]
duplicar [] = []
duplicar xs =  xs ++ [0] ++ (map ( *2 )  xs)

nElementosAlFinal :: [a] -> Int -> [a]
nElementosAlFinal [] _ = []
nElementosAlFinal xs 0 = xs
nElementosAlFinal (x:xs) a = nElementosAlFinal (xs ++ [x]) (a-1)

sumarNprimeros :: Num a =>  [a] -> Int -> a
sumarNprimeros [] _ = 0
sumarNprimeros _ 0 = 0
sumarNprimeros (x:xs) a = x + sumarNprimeros xs (a-1)
 
restarAnterioresA :: Num a =>  [a] -> Int -> a
restarAnterioresA [] _ = 0
restarAnterioresA _ 0 = 0
restarAnterioresA xs a = - sumarLista(take a xs)


sumatoria :: Integral a =>  a -> a 
sumatoria n = div (n * (n + 1) ) 2


sumatoriaManual :: Int -> Int
sumatoriaManual 0 = 0
sumatoriaManual a = a + sumatoriaManual (a-1)


sum_k_n_Listas :: Int -> Int -> Int
sum_k_n_Listas k n = sum [k.. (n - 1)]

sum_k_n :: Int -> Int -> Int
sum_k_n k n = 
    if k > n then 0
    else div (n * (n + 1) ) 2 - ( div (k * (k - 1) )  2)
    -- o llamar a sumatoria n -- resto del a fórmula

numbersStreet :: Int -> [Int] -> Bool
numbersStreet a (x:xs) = sum_k_n 1 (a-1) == sum_k_n (a+1) (last xs)

k = 6
[1,2,3,4,5,6,7,8]

esPar :: Int -> Bool
esPar n = mod n 2 == 0

contarPares :: Int -> Int -> Int
contarPares a b = if a > b then 0 
                else if esPar a then 1 + contarPares (a + 1) b
                else contarPares (a + 1) b
