import System.Random
import System.Exit (exitSuccess)
import System.IO
import Data.List


main :: IO ()
main = do
  putStrLn "Adivinanza de números"
  putStrLn "1. Jugar"
  putStrLn "2. Ver puntuaciones"
  putStrLn "3. Salir"
  putStr "Seleccione una opcion: "
  opcion <- getLine 

  case opcion of 
    "1" -> jugar
    "2" -> mostrarPuntuaciones
    "3" -> salirAdivinanza
    eleccion -> putStr "Seleccione una opción válida"
  main

-- Funciones del menú principal
jugar :: IO()
jugar = do 
    putStr "\nEscriba su nombre de jugador: "
    nombre <- getLine 
    putStr "Límite inferior para generar el número: "
    input <- getLine 
    let n = read input :: Int
    putStr "Límite superior para generar el número: "
    input <- getLine 
    let m = read input :: Int
    rn <- numeroAleatorio n m
    let numAleatorio =  rn :: Int
    let intentos = calcularIntentos n m
    let valores = []
    juegoRecursivo nombre rn 1 intentos valores

juegoRecursivo:: String -> Int-> Int-> Int -> [Int] -> IO()
juegoRecursivo nombre numAleatorio intento intentolimit listanum
  | intento <=intentolimit = do
      putStr "Intento número " >> print intento
      putStr "Teclee un número: "
      input <- getLine 
      let numEntrada = read input :: Int
      let listanum2 = append numEntrada listanum
      case compare numEntrada numAleatorio of 
        EQ -> do
          putStrLn "Acertaste"
          putStrLn "Cantidad de intentos: " >> print intento
          let puntuacion = calcularPuntuacion intento intentolimit 0 True numAleatorio
          putStrLn "Su puntuación: "
          putStrLn "Jugador: " >>print nombre >> putStr "puntuación: ">> print puntuacion >> putStrLn "\n"
          anadirPuntuacion nombre puntuacion
        GT -> do
          putStrLn "El número que ingresaste es más alto que el aleatorio \n"
          juegoRecursivo nombre numAleatorio (intento+1) intentolimit listanum2
        LT -> do
          putStrLn "El número que ingresaste es más bajo que el aleatorio \n"
          juegoRecursivo nombre numAleatorio (intento+1) intentolimit listanum2
  | otherwise = do
    putStrLn "Se acabaron tus intentos"
    putStrLn "El número era: " >>print numAleatorio 
    let numcercano = numCercano numAleatorio listanum
    let puntuacion = calcularPuntuacion intento intentolimit numcercano False numAleatorio
    putStrLn "Su puntuación: ">> print puntuacion >> putStrLn "\n"
    anadirPuntuacion nombre puntuacion 

mostrarPuntuaciones :: IO ()
mostrarPuntuaciones = do
  puntuaciones <- openFile "puntuaciones.txt" ReadMode
  contenido <- hGetContents puntuaciones
  let renglones = lines contenido
  putStrLn "Jugador y sus respectivas puntuaciones"
  mapM_ putStrLn renglones
  putStrLn "\n\n\n"
  hClose puntuaciones
  
salirAdivinanza :: IO b
salirAdivinanza = do 
  putStrLn "Saliendo del juego"
  exitSuccess

-- Funciones auxiliares de las demás funciones
anadirPuntuacion::  String -> Int -> IO ()
anadirPuntuacion nombre numero = do
  appendFile "puntuaciones.txt" (nombre ++ "\t\t" ++ show numero ++ "\n") 

calcularPuntuacion:: Int -> Int -> Int -> Bool -> Int -> Int 
calcularPuntuacion intento intentolimit cercano acerto aleatorio
  | acerto = calcAcierto intento intentolimit 
  | otherwise = calcFallo intentolimit cercano aleatorio 

calcAcierto:: Int -> Int -> Int 
calcAcierto intento intentolimit 
  | intentolimit == 10 = 100 - (intento*2)
  | intentolimit == 5 = 100 - (intento*5)
  | otherwise = 100 - (intento * 7)

calcFallo:: Int -> Int -> Int -> Int 
calcFallo intentolimit cercano aleatorio 
  | intentolimit == 10 && abs(aleatorio-cercano) == 1 = 30
  | intentolimit == 10 && abs(aleatorio-cercano) <= 5 = 15
  | intentolimit == 10 && abs(aleatorio-cercano) <= 1 = 10
  | intentolimit == 5 && abs(aleatorio-cercano) == 1 = 20
  | intentolimit == 5 && abs(aleatorio-cercano) <= 5 = 10
  | intentolimit == 10 && abs(aleatorio-cercano) == 1 = 15
  | intentolimit == 10 && abs(aleatorio-cercano) <= 3 = 5
  | otherwise = 0

numeroAleatorio :: Int -> Int -> IO Int
numeroAleatorio n m = randomRIO(n,m)

calcularIntentos :: Int -> Int -> Int 
calcularIntentos n m 
    | m-n <= 5 = 3
    | m-n <=30 = 5
    | otherwise = 10

append :: Int -> [Int] -> [Int]
append n1 [] = [n1]
append n1 (x:xs) = x : append n1 xs

numCercano :: (Num a, Ord a) => a -> [a] -> a
numCercano x xs = cercano x xs (head xs)

cercano :: (Num a, Ord a) => a -> [a] -> a -> a
cercano _ [] c = c
cercano x (y:ys) c
    | abs (y - x) < abs (c - x) = cercano x ys y
    | otherwise = cercano x ys c