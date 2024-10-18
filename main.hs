module Main  where
import Funciones
import Data.List (sort)
import Punto
import Interludio

estaIncluida :: Eq a => [a] -> [a] -> Bool
estaIncluida [] _ = True
estaIncluida _ [] = False
estaIncluida xs ys = if esPrefijo xs ys
                     then True
                     else estaIncluida xs (tail ys)

esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] _ = True
esPrefijo _ [] = False
esPrefijo (x:xs) (y:ys) = if x == y
                           then esPrefijo xs ys
                           else False

esMultiplo :: Int -> Int -> Int
esMultiplo x y =
  if mod x y == 0
    then 1
    else 0

multiplos_de_entre :: Int -> Int -> Int -> Int
multiplos_de_entre f ini fin = 
  if ini > fin then 0
  else if mod ini f == 0
      then 1 + multiplos_de_entre f (ini + 1) fin
      else multiplos_de_entre f (ini + 1) fin




miReverse :: [a] -> [a]
miReverse [] = []
miReverse (x:xs) = miReverse xs ++ [x]


esPalindromo2 :: String -> Bool
esPalindromo2 [] = True
esPalindromo2 [x] = True
esPalindromo2 (x:xs) = x == last xs && esPalindromo2 (init xs)

maxNLista :: Ord a => [a] -> Int -> [a]
maxNLista [x] n = [x]
maxNLista xs n = take n (miReverse (sort xs))

--maxNLista (x:xs) n = ([] ++ reverse (sort(xs)) )  ++ reverse(sort(xs))
--hacerlo sin take
    --
eliminarDuplicados :: [Int] -> [Int]
eliminarDuplicados [] = []
eliminarDuplicados [x] = [x]
eliminarDuplicados (x:xs) = if elem x xs
  then eliminarDuplicados xs
  else x : eliminarDuplicados xs

main :: IO ()
main = do
    let lista = [5, 8, 3, 12, 6, 9]
        maximos = maxNLista lista 4
    putStrLn "Lista original:"
    print lista
    putStrLn "Dos valores más grandes:"
    print maximos
