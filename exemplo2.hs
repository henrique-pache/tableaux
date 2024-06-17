-- exemplo2.hs
import FormulaLogic (evaluate, buildTree, Formula(..))

main :: IO ()
main = do
    let formula2 = Not (Or (Atom True) (Not (Atom True)))

    putStrLn "Exemplo 2:"
    putStrLn $ "Fórmula: " ++ show formula2
    putStrLn $ "Avaliação: " ++ show (evaluate formula2)
    putStrLn $ "Árvore de Tableaux: " ++ buildTree formula2
