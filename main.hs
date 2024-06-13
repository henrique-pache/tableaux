-- Definindo a estrutura de dados para representar fórmulas lógicas
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

-- Função auxiliar para avaliar átomos (simulação de tabela verdade)
evalAtom :: String -> Bool
evalAtom "True" = True
evalAtom "False" = False
evalAtom _   = error "Atom deve ser 'True' ou 'False'"

-- Função para construir uma árvore de tableaux (opcional, apenas para demonstração)
buildTree :: Formula -> String
buildTree (Atom True) = "True"
buildTree (Atom False) = "False"
buildTree (Not formula) = "¬" ++ buildTree formula
buildTree (And left right) = "(" ++ buildTree left ++ " ∧ " ++ buildTree right ++ ")"
buildTree (Or left right) = "(" ++ buildTree left ++ " ∨ " ++ buildTree right ++ ")"
buildTree (Imply left right) = "(" ++ buildTree left ++ " → " ++ buildTree right ++ ")"
buildTree (BiImply left right) = "(" ++ buildTree left ++ " ↔ " ++ buildTree right ++ ")"

-- Função principal para demonstrar a avaliação de fórmulas lógicas
main :: IO ()
main = do
    let formula1 = Or (Atom True) (And (Atom False) (Or (Atom True) (Atom False)))
    let formula2 = Not (Or (Atom True) (Not (Atom True)))
    let formula3 = Imply (Atom True) (Atom False)
    let formula4 = BiImply (Atom True) (Not (Atom False))
    let formula5 = BiImply (Atom True) (Imply (Atom False) (Atom True))

    putStrLn "Exemplo 1:"
    putStrLn $ "Fórmula: " ++ show formula1
    putStrLn $ "Avaliação: " ++ show (evaluate formula1)
    putStrLn $ "Árvore de Tableaux: " ++ buildTree formula1
    putStrLn ""

    putStrLn "Exemplo 2:"
    putStrLn $ "Fórmula: " ++ show formula2
    putStrLn $ "Avaliação: " ++ show (evaluate formula2)
    putStrLn $ "Árvore de Tableaux: " ++ buildTree formula2
    putStrLn ""

    putStrLn "Exemplo 3:"
    putStrLn $ "Fórmula: " ++ show formula3
    putStrLn $ "Avaliação: " ++ show (evaluate formula3)
    putStrLn $ "Árvore de Tableaux: " ++ buildTree formula3
    putStrLn ""

    putStrLn "Exemplo 4:"
    putStrLn $ "Fórmula: " ++ show formula4
    putStrLn $ "Avaliação: " ++ show (evaluate formula4)
    putStrLn $ "Árvore de Tableaux: " ++ buildTree formula4
    putStrLn ""

    putStrLn "Exemplo 5:"
    putStrLn $ "Fórmula: " ++ show formula5
    putStrLn $ "Avaliação: " ++ show (evaluate formula5)
    putStrLn $ "Árvore de Tableaux: " ++ buildTree formula5
    putStrLn ""
