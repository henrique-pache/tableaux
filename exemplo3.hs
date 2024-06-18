-- exemplo3.hs
import FormulaLogic (evaluate, buildTree, Formula(..))

main :: IO ()
main = do
    let formula3 = Imply (Atom True) (Atom False)

    putStrLn "Exemplo 3:"
    putStrLn $ "Fórmula: " ++ show formula3
    putStrLn $ "Avaliação: " ++ show (evaluate formula3)
    putStrLn $ "Árvore de Tableaux:\n " ++ buildTree formula3
