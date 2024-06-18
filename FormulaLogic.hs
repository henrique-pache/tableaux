-- FormulaLogic.hs
module FormulaLogic (
    Formula(..),
    evaluate,
    buildTree
) where

-- Definição da estrutura de dados para representar fórmulas lógicas
data Formula = Atom Bool
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imply Formula Formula   -- Implicação
             | BiImply Formula Formula -- Bi-implicação
             deriving (Show)

-- Função para avaliar uma fórmula lógica
evaluate :: Formula -> Bool
evaluate (Atom b) = b
evaluate (Not formula) = not (evaluate formula)
evaluate (And left right) = evaluate left && evaluate right
evaluate (Or left right) = evaluate left || evaluate right
evaluate (Imply left right) = not (evaluate left) || evaluate right
evaluate (BiImply left right) = (evaluate left == evaluate right)

-- Função para construir uma árvore de tableaux (opcional, apenas para demonstração)
buildTree :: Formula -> String
buildTree formula = buildTreeIndented formula 0

-- Função auxiliar para construir a árvore com indentação
buildTreeIndented :: Formula -> Int -> String
buildTreeIndented (Atom True) _ = "True"
buildTreeIndented (Atom False) _ = "False"
buildTreeIndented (Not formula) depth =
    indent depth ++ "¬" ++ buildTreeIndented formula (depth + 1)
buildTreeIndented (And left right) depth =
    indent depth ++ "∧\n" ++
    indent (depth + 1) ++ buildTreeIndented left (depth + 1) ++ "\n" ++
    indent (depth + 1) ++ buildTreeIndented right (depth + 1)
buildTreeIndented (Or left right) depth =
    indent depth ++ "∨\n" ++
    indent (depth + 1) ++ buildTreeIndented left (depth + 1) ++ "\n" ++
    indent (depth + 1) ++ buildTreeIndented right (depth + 1)
buildTreeIndented (Imply left right) depth =
    indent depth ++ "→\n" ++
    indent (depth + 1) ++ buildTreeIndented left (depth + 1) ++ "\n" ++
    indent (depth + 1) ++ buildTreeIndented right (depth + 1)
buildTreeIndented (BiImply left right) depth =
    indent depth ++ "↔\n" ++
    indent (depth + 1) ++ buildTreeIndented left (depth + 1) ++ "\n" ++
    indent (depth + 1) ++ buildTreeIndented right (depth + 1)

-- Função para gerar a indentação com base na profundidade
indent :: Int -> String
indent depth = replicate (depth * 2) ' '
