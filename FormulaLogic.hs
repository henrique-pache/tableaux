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
buildTree (Atom True) = "True"
buildTree (Atom False) = "False"
buildTree (Not formula) = "¬" ++ buildTree formula
buildTree (And left right) = "(" ++ buildTree left ++ " ∧ " ++ buildTree right ++ ")"
buildTree (Or left right) = "(" ++ buildTree left ++ " ∨ " ++ buildTree right ++ ")"
buildTree (Imply left right) = "(" ++ buildTree left ++ " → " ++ buildTree right ++ ")"
buildTree (BiImply left right) = "(" ++ buildTree left ++ " ↔ " ++ buildTree right ++ ")"
