-- exemplo1.hs
import FormulaLogic (evaluate, buildTree, Formula(..))

main :: IO ()
main = do
    let formula1 = Or (Atom True) (And (Atom False) (Or (Atom True) (Atom False)))

    putStrLn "Exemplo 1:"
    putStrLn $ "Fórmula: " ++ show formula1
    putStrLn $ "Avaliação: " ++ show (evaluate formula1)
    putStrLn $ "Árvore de Tableaux: " ++ buildTree formula1
