import Data.List (intercalate)

data Expr
    = Var String
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Implies Expr Expr
    deriving (Show, Eq)

data Tableaux = Node [Expr] [Tableaux] | Leaf Bool
    deriving (Show, Eq)

expand :: [Expr] -> [[Expr]]
expand [] = []
expand (Var x : es) = [es]
expand (Not (Var x) : es) = [es]
expand (Not (Not e) : es) = [e : es]
expand (And e1 e2 : es) = [[e1, e2] ++ es]
expand (Or e1 e2 : es) = [[e1] ++ es, [e2] ++ es]
expand (Implies e1 e2 : es) = [[Not e1] ++ es, [e2] ++ es]
expand (Not (And e1 e2) : es) = [[Not e1] ++ es, [Not e2] ++ es]
expand (Not (Or e1 e2) : es) = [[Not e1, Not e2] ++ es]
expand (Not (Implies e1 e2) : es) = [[e1, Not e2] ++ es]

buildTableaux :: [Expr] -> Tableaux
buildTableaux [] = Leaf True
buildTableaux exprs
    | any isContradiction exprs = Leaf False
    | otherwise = Node exprs (map buildTableaux (expand exprs))
  where
    isContradiction (Var x) = Not (Var x) `elem` exprs
    isContradiction (Not (Var x)) = Var x `elem` exprs
    isContradiction _ = False

checkValidity :: Tableaux -> Bool
checkValidity (Leaf b) = b
checkValidity (Node _ children) = any checkValidity children

isValid :: Expr -> (Bool, Tableaux)
isValid expr =
    let tableaux = buildTableaux [Not expr]
        validity = not (checkValidity tableaux)
    in (validity, tableaux)

formatTableaux :: Tableaux -> String
formatTableaux (Leaf b) = if b then "Closed" else "Open"
formatTableaux (Node exprs children) =
    let exprStr = intercalate ";" (map showExpr exprs)
        childrenStr = intercalate "@" (map formatSubtree children)
    in exprStr ++ "@" ++ childrenStr
  where
    showExpr :: Expr -> String
    showExpr (Var x) = x
    showExpr (Not e) = "¬" ++ showExpr e
    showExpr (And e1 e2) = "(" ++ showExpr e1 ++ " ∧ " ++ showExpr e2 ++ ")"
    showExpr (Or e1 e2) = "(" ++ showExpr e1 ++ " ∨ " ++ showExpr e2 ++ ")"
    showExpr (Implies e1 e2) = "(" ++ showExpr e1 ++ " → " ++ showExpr e2 ++ ")"

    formatSubtree :: Tableaux -> String
    formatSubtree t = "{" ++ formatTableaux t ++ "}"

main :: IO ()
main = do
    let expr1 = Implies (Var "p") (Var "p")
    let expr2 = And (Var "p") (Not (Var "p"))
    let (valid1, tableaux1) = isValid expr1
    let (valid2, tableaux2) = isValid expr2
    putStrLn $ "Expr1: " ++ show expr1
    putStrLn $ "Valid: " ++ show valid1
    putStrLn $ "Tableaux: " ++ formatTableaux tableaux1
    putStrLn ""
    putStrLn $ "Expr2: " ++ show expr2
    putStrLn $ "Valid: " ++ show valid2
    putStrLn $ "Tableaux: " ++ formatTableaux tableaux2
