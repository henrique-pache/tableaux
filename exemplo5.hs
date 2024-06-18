-- exemplo5.hs
import FormulaLogic (evaluate, buildTree, Formula(..))

main :: IO ()
main = do
    let formula5 = BiImply (Atom True) (Imply (Atom False) (Atom True))

    putStrLn "Exemplo 5:"
    putStrLn $ "Fórmula: " ++ show formula5
    putStrLn $ "Avaliação: " ++ show (evaluate formula5)
    putStrLn $ "Árvore de Tableaux:\n " ++ buildTree formula5
