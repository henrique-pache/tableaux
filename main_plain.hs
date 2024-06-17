-- Arquivo Main.hs

module Main where

import Formula (Formula(..), evaluate, buildTree)  -- Importando definições de fórmulas

import Example1 (example1)  -- Importando chamada do exemplo 1
import Example2 (example2)  -- Importando chamada do exemplo 2

-- Definições das fórmulas e lógica de avaliação

-- Definição da fórmula específica para exemplo 1
formula1 :: Formula
formula1 = Or (Atom True) (And (Atom False) (Or (Atom True) (Atom False)))

-- Definição da fórmula específica para exemplo 2
formula2 :: Formula
formula2 = Not (Or (Atom True) (Not (Atom True)))

-- Função principal para demonstrar a avaliação de fórmulas lógicas
main :: IO ()
main = do
    putStrLn "Executando exemplos:"
    example1 formula1
    example2 formula2
