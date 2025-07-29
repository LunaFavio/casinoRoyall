module Library where
import PdePreludat

--Casino Royal

palos :: [String]
palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]

type Carta = (Number, String)

data Jugador = Jugador {
nombre :: String,
mano :: [Carta],
bebida :: String
} deriving (Show, Eq)

pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]

jamesBond   = Jugador "Bond... James Bond" pokerDeAses "Martini... shaken, not stirred"
leChiffre   = Jugador "Le Chiffre" fullDeJokers "Gin"
felixLeiter = Jugador "Felix Leiter" piernaDeNueves "Whisky"

mesaQueMasAplauda = [jamesBond, leChiffre, felixLeiter]

ocurrenciasDe x = length . filter (== x)
concatenar = foldl (++) []

--1
--A) mayorSegun/3, que dada una función y dos valores nos devuelve aquel 
--   valor que hace mayor a la función (en caso de igualdad, cualquiera de los dos).

mayorSegun f v1 v2 
    | f v1 > f v2 = v1
    | otherwise   = v2

--B) maximoSegun/2, que dada una función y una lista de valores nos devuelve aquel 
--   valor de la lista que hace máximo a la función. No usar recursividad.

maximoSegun f lista = foldl1 (mayorSegun f) lista 

--C) sinRepetidos/1, que dada una lista devuelve la misma sin elementos repetidos. 
--   Los elementos tienen que aparecer en el mismo orden que en la lista original
--  (la primera ocurrencia de la lista de izquierda a derecha).

sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

--2)
--A) esoNoSeVale/1, que se cumple para una carta inválida, 
--   ya sea por número o por palo (ver arriba cómo tiene que ser una carta).

esoSeVale :: Carta -> Bool
esoSeVale (numero, palo) = elem palo palos && elem numero [1..13]

esoNoSeVale :: Carta -> Bool
esoNoSeVale (numero, palo) = not(esoSeVale(numero,palo))

--B) manoMalArmada/1, que dado un jugador, nos indica si tiene una mano mal armada.
--   Esto es cuando sus cartas no son exactamente 5, o alguna carta es inválida.

manoMalArmada (Jugador _ mano _) = ((/= 5) . length) mano || any esoNoSeVale mano

--3)
-- Dada una lista de cartas, hacer las funciones que verifican si las 
-- mismas forman un juego dado, según las siguientes definiciones:
type Juego = [Carta] -> Bool

repeticionesEnMano repeticiones cartas =
    elem repeticiones (repeticionNumeros numerosEnMano)
    where numerosEnMano = map fst cartas

repeticionNumeros numeros = map (`ocurrenciasDe` numeros) numeros

repeticionesEnMano2 repeticiones cartas = 
  any ((== repeticiones). flip ocurrenciasDe numerosEnMano) numerosEnMano
  where numerosEnMano = map fst cartas

-- a) par --> tiene un número que se repite 2 veces
par cartas = repeticionesEnMano 2 cartas

-- b) pierna --> tiene un número que se repite 3 veces
pierna cartas = repeticionesEnMano 3 cartas

-- c) color -->  todas sus cartas son del mismo palo
color cartas = all (== head palosEnMano) palosEnMano
    where palosEnMano = map snd cartas

-- d) fullHouse --> es, a la vez, par y pierna
fullHouse cartas = pierna cartas && par cartas

-- e) poker --> tiene un número que se repite 4 veces
poker cartas = repeticionesEnMano 3 cartas

-- f) otro --> se cumple para cualquier conjunto de cartas
otro :: Juego
otro _ = True

-- 4) 
-- alguienSeCarteo/1, dada una lista de jugadores. Sabemos que alguien se carteó 
-- cuando hay alguna carta que se repite, ya sea en un mismo jugador o en distintos.

alguienSeCarteo :: [Jugador] -> Bool
alguienSeCarteo jugadores =
    any ((>=2).flip ocurrenciasDe cartasEnJuego) cartasEnJuego
    where cartasEnJuego = concat $ map mano jugadores

alguienSeCarteo2 jugadores =
    cartasEnJuego /= sinRepetidos cartasEnJuego
    where cartasEnJuego = concat $ map mano jugadores

--5)
valores = [(par,1), (pierna,2), (color,3), (fullHouse,4), (poker,5), (otro, 0)]

--a) Definir valor/1 que, dada una lista de cartas, nos indique el valor de la misma, 
--   que es el máximo valor entre los juegos que la lista de cartas cumple.
valor cartas =
    maximum $ map snd valoresEnJuego
    where valoresEnJuego = filter (\v -> fst v == True) $ map (\(j,v)-> (j cartas, v)) valores

valor2 cartas =
    snd . maximoSegun snd $ filter (\(j,v) -> j cartas) valores

valor3 cartas =
  snd . maximoSegun snd $ filter (($ cartas).juego) valores
  where juego = fst
  -- maximum . map valorJuego $ (filter (($ cartas).juego) valores)

--b) bebidaWinner/1, que dada una lista de jugadores nos devuelve la bebida de aquel jugador 
--   que tiene el juego de mayor valor, pero sin considerar a aquellos que tienen manos mal armadas.

type Mesa = [Jugador]

bebidaWinner jugadores =
    bebida $ maximoSegun (valor3.mano) jugadoresValidos
    where jugadoresValidos = filter (not.manoMalArmada) jugadores

--6)
-- a) El nombre del jugador que está tomando la bebida de nombre más largo.
-- nombre $ maximoSegun (length.bebida) jugadores

--b) El jugador con mayor cantidad de cartas inválidas.
-- maximoSegun (length.filter esoNoSeVale.mano) jugadores

--c) El jugador de nombre más corto.
-- maximoSegun (negate.length.nombre) jugadores

--d) El nombre del ganador de una mesa, que es aquel del jugador con el juego de mayor valor.
-- nombre $ maximoSegun (valor3.mano) jugadores

--7)
--a) Implementar una función de ordenamiento ordenar/2, que dado un criterio y 
--   una lista me devuelva la misma lista con sus elementos ordenados en base al criterio.

ordenar _ [] = []

ordenar criterio (x:xs) = 
    (ordenar criterio . filter (not. criterio x) $ xs) ++ [x] ++ 
    (ordenar criterio . filter (criterio x) $ xs)

--b)
--1) Una escalera es una sucesión de números con las 5 cartas. 
--   ¡Cuidado! Las cartas pueden estar desordenadas. 
--   Por ejemplo, si tengo las cartas 6, 3, 4, 2, 5 (no importa el palo) 
--   eso es una escalera porque tiene las cartas del 2 al 6.

escalera :: Juego
escalera cartas =
    all (\(a,b)-> b == a + 1) (zip numerosOrdenados (tail numerosOrdenados))
    where numerosOrdenados = ordenar (<=) (map fst cartas)
    
--2) Una escaleraDeColor es como una escalera pero que tiene todas las cartas del mismo palo.

--escaleraDeColor :: Juego
--escaleraDeColor cartas = escalera cartas && color cartas
escaleraDeColor2 cartas = all ($ cartas) [escalera, color]