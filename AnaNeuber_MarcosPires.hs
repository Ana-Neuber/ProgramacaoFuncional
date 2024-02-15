{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use when" #-}

import System.Random

aleatorio :: IO Int
aleatorio = randomRIO (1, 100) :: IO Int

atualiza_recorde :: Int -> IO()
atualiza_recorde tentativas = do
  recorde <- readFile "highscore.txt"
  if tentativas < (read recorde :: Int)
    then do
      putStrLn "Você bateu o recorde!"
      writeFile "highscore.txt" (show tentativas)
    else return()

compara_palpite :: Int -> Int -> String
compara_palpite palpite numero = if palpite > numero then "acima" else "abaixo"

jogo :: Int -> Int -> IO()
jogo tentativas numero = do
  putStrLn ("\n== Tentativa " ++ show tentativas ++ " ===")
  putStr "Digite seu palpite: "
  palpite <- getLine
  if (read palpite :: Int) == numero
    then do 
      putStrLn ("\nParabens! Você acertou em " ++ show tentativas ++ " tentativas.")
      atualiza_recorde tentativas
    else do
        putStrLn ("Seu palpite de " ++ show palpite ++ " está " ++ compara_palpite (read palpite :: Int) numero ++ " do número correto.")
        jogo (tentativas + 1) numero

                
main :: IO()
main = do
  numero <- aleatorio
  putStrLn "\n\nBem-vindo ao Jogo de Adivinhação!"
  putStrLn "Estou pensando em um número entre 1 e 100. Tente adivinhar.\n"
  jogo 1 numero
  
