module Main where

esMultipl :: Int -> Int -> Bool
esMultipl x y = if mod x y == 0 then True else False

esImpar :: Int -> Bool
esImpar x = x `mod` 2 /= 0