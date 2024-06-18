-- exemplo4.hs
import FormulaLogic (evaluate, buildTree, Formula(..))

main :: IO ()
main = do
    let formula4 = BiImply (Atom True) (Not (Atom False))

    putStrLn "Exemplo 4:"
    putStrLn $ "Fórmula: " ++ show formula4
    putStrLn $ "Avaliação: " ++ show (evaluate formula4)
    putStrLn $ "Árvore de Tableaux:\n " ++ buildTree formula4
