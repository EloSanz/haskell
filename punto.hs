module Punto where
import Funciones

data Punto = Punto {x :: Double , y :: Double }
    deriving (Show)

sumarPuntos::Punto -> Punto ->Punto
sumarPuntos p1 p2 = 
    Punto{x = x p1 + x p2, y = y p1 + y p2 }
    
calcularDistancia::Punto -> Punto -> Double
calcularDistancia p1 p2 = 
    sqrt ( pow (x p1 - x p2)  2  +  pow (y p1 - y p2)  2 )