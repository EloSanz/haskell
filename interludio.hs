module Interludio  where
--fold, practicar

--patter matchin
quitaTres :: [a] ->[a]
quitaTres (_:_:_:xs) = xs
quitaTres _ = []

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1)

-- para agregar elementos a una lista es :
--x = 2
--xs = [2,3]
--x:xs = [1,2,3]

enqueue :: a -> [a] -> [a]
enqueue a xs = xs ++ [a]

dequeue :: [a] -> (a, [a])
dequeue [] = error "cola vacia"
dequeue (x:xs) = (x, xs)

push :: a -> [a] -> [a]
push a xs = a:xs


eliminarElemento :: Eq a => a -> [a] -> [a]
eliminarElemento x [] = []  -- Caso base: si la lista está vacía, devuelve una lista vacía.
eliminarElemento x (y:ys) = if y== x 
                            then eliminarElemento x ys
                            else y : eliminarElemento x ys
                            

