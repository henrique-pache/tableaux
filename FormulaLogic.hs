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

-- Função para construir uma árvore de prova/refutação
buildTree :: Formula -> String
buildTree formula = buildTreeIndented formula True 0

-- Função auxiliar para construir a árvore com indentação
buildTreeIndented :: Formula -> Bool -> Int -> String
buildTreeIndented (Atom b) isProved depth =
    indent depth ++ if b then "Atom True" else "Atom False"
buildTreeIndented (Not formula) isProved depth =
    let prefix = if isProved then "¬ (Prove)" else "¬ (Refute)"
    in indent depth ++ prefix ++ "\n" ++
       buildTreeIndented formula (not isProved) (depth + 1)
buildTreeIndented (And left right) isProved depth =
    let prefix = if isProved then "∧ (Prove)" else "∧ (Refute)"
    in indent depth ++ prefix ++ "\n" ++
       buildTreeIndented left isProved (depth + 1) ++ "\n" ++
       buildTreeIndented right isProved (depth + 1)
buildTreeIndented (Or left right) isProved depth =
    let prefix = if isProved then "∨ (Prove)" else "∨ (Refute)"
    in indent depth ++ prefix ++ "\n" ++
       buildTreeIndented left isProved (depth + 1) ++ "\n" ++
       buildTreeIndented right isProved (depth + 1)
buildTreeIndented (Imply left right) isProved depth =
    let prefix = if isProved then "→ (Prove)" else "→ (Refute)"
    in indent depth ++ prefix ++ "\n" ++
       buildTreeIndented left (not isProved) (depth + 1) ++ "\n" ++
       buildTreeIndented right isProved (depth + 1)
buildTreeIndented (BiImply left right) isProved depth =
    let prefix = if isProved then "↔ (Prove)" else "↔ (Refute)"
    in indent depth ++ prefix ++ "\n" ++
       buildTreeIndented left isProved (depth + 1) ++ "\n" ++
       buildTreeIndented right isProved (depth + 1)

-- Função para gerar a indentação com base na profundidade
indent :: Int -> String
indent depth = replicate (depth * 2) ' '

-- Exemplo de uso
main :: IO ()
main = do
    let formula = And (Not (Atom True)) (Or (Atom False) (Atom True))
    putStrLn (buildTree formula)
