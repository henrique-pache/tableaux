import Data.List (nub)

-- Definição das Fórmulas Lógicas
data Formula = Var String
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imply Formula Formula
             deriving (Eq)

instance Show Formula where
    show (Var x) = x
    show (Not f) = "~" ++ show f
    show (And f1 f2) = "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
    show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
    show (Imply f1 f2) = "(" ++ show f1 ++ " → " ++ show f2 ++ ")"

-- Regras de Expansão de Tableaux
expand :: Formula -> [Formula] -> [[Formula]]
expand (Not (Not p)) f = [p : f]
expand (And p q) f = [[p, q] ++ f]
expand (Or p q) f = [[p] ++ f, [q] ++ f]
expand (Imply p q) f = [[Not p] ++ f, [q] ++ f]
expand (Not (And p q)) f = [[Not p] ++ f, [Not q] ++ f]
expand (Not (Or p q)) f = [[Not p, Not q] ++ f]
expand (Not (Imply p q)) f = [[p, Not q] ++ f]
expand _ f = [f]

-- Método de Tableaux
tableaux :: [Formula] -> [[Formula]]
tableaux [] = [[]]
tableaux (f:fs) =
    case f of
        Var _      -> map (f:) (tableaux fs)
        Not (Var _) -> map (f:) (tableaux fs)
        _          -> concatMap (\b -> tableaux (b ++ fs)) (expand f [])

-- Verificação de Contradições
isContradiction :: [Formula] -> Bool
isContradiction fs = any (\v -> (Not v) `elem` fs) fs

-- Filtragem de Ramos Não Contraditórios
filterNonContradictory :: [[Formula]] -> [[Formula]]
filterNonContradictory = filter (not . isContradiction)

-- Função Principal
main :: IO ()
main = do
    -- Defina sua fórmula lógica aqui
    let formula = Imply (Var "A") (Imply (Var "B") (Var "A"))
    -- Executa o método de tableaux
    let result = filterNonContradictory (tableaux [Not formula])
    -- Verifica e imprime o resultado
    if null result
        then putStrLn "Válida"
        else do
            putStrLn "Inválida"
            mapM_ (putStrLn . formatBranch) result

-- Formatação dos Ramos da Árvore
formatBranch :: [Formula] -> String
formatBranch = unwords . map show
