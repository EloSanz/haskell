module Funciones where

pow :: (Num a, Integral b) => a -> b -> a
pow _ 0 = 1
pow a b = a * pow a (b - 1) 

cuadrado :: Int -> Int
cuadrado x = x * x

cuadradoD::Double -> Double
cuadradoD x = x * x


esPositivo :: Int -> Bool
esPositivo x = if x > 0 
  
 

maximoLista :: Ord a => [a] -> a
maximoLista [x] = x --caso base
maximoLista (x:xs) = max x ( maximoLista(xs) )

minimoLista :: Ord min => [min] -> min
minimoLista [uno] = uno
minimoLista (x:cola) = min x (minimoLista(cola)) 

sumarEnLista :: Num a => [a] -> a
sumarEnLista [] = 0
sumarEnLista (x:xs) = x + (sumarEnLista xs)

esDivisible :: Integral a => a -> a -> Bool
esDivisible a b = a `mod` b == 0

listaConMultiplosDe :: Integral a => a -> [a] -> [a]
listaConMultiplosDe m [] = []
listaConMultiplosDe m (x:xs) = 
  if mod x m == 0
    then x : listaConMultiplosDe m xs
    else listaConMultiplosDe m xs
    
