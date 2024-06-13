-- Arquivo de exemplo para o Tableaux

import Data.List (nub)

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

expand :: Formula -> [Formula] -> [[Formula]]
expand (Not (Not p)) f = [p : f]
expand (And p q) f = [[p, q] ++ f]
expand (Or p q) f = [[p] ++ f, [q] ++ f]
expand (Imply p q) f = [[Not p] ++ f, [q] ++ f]
expand (Not (And p q)) f = [[Not p] ++ f, [Not q] ++ f]
expand (Not (Or p q)) f = [[Not p, Not q] ++ f]
expand (Not (Imply p q)) f = [[p, Not q] ++ f]
expand _ f = [f]

tableaux :: [Formula] -> [[Formula]]
tableaux [] = [[]]
tableaux (f:fs) =
    case f of
        Var _      -> map (f:) (tableaux fs)
        Not (Var _) -> map (f:) (tableaux fs)
        _          -> concatMap (\b -> tableaux (b ++ fs)) (expand f [])

isContradiction :: [Formula] -> Bool
isContradiction fs = any (\v -> (Not v) `elem` fs) fs

filterNonContradictory :: [[Formula]] -> [[Formula]]
filterNonContradictory = filter (not . isContradiction)

-- Fórmulas de exemplo
exemplos :: [[Formula]]
exemplos =
    [ [Imply (Var "P") (Var "Q")]     -- Exemplo 1
    , [Not (And (Var "P") (Var "Q"))] -- Exemplo 2
    , [Or (Var "P") (Not (Var "P"))]  -- Exemplo 3 (Contradição)
    ]

main :: IO ()
main = do
    putStrLn "Exemplos de fórmulas:"
    mapM_ print exemplos

    putStrLn "\nResultados:"
    mapM_ (\f -> do
                let result = filterNonContradictory (tableaux [Not f])
                putStrLn $ if null result then "Válida" else "Inválida"
                mapM_ (putStrLn . formatBranch) result
          ) (concat exemplos)

formatBranch :: [Formula] -> String
formatBranch = unwords . map show
