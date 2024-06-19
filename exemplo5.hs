-- exemplo5.hs
import FormulaLogic (evaluate, buildTree, Formula(..))

main :: IO ()
main = do
    let formula10 = BiImply
                    (Imply
                        (Or
                            (Atom True)
                            (Not (Atom False))
                        )
                        (And
                            (Not (Atom True))
                            (Or
                                (Atom False)
                                (And
                                    (Atom True)
                                    (Not (Atom False))
                                )
                            )
                        )
                    )
                    (Or
                        (Not
                            (And
                                (Atom True)
                                (Not (Atom False))
                            )
                        )
                        (Or
                            (And
                                (Atom True)
                                (Or
                                    (Atom False)
                                    (Atom True)
                                )
                            )
                            (Imply
                                (Atom False)
                                (Not
                                    (Or
                                        (Atom True)
                                        (Not (Atom False))
                                    )
                                )
                            )
                        )
                    )

    putStrLn "Exemplo 10:"
    putStrLn $ "Fórmula: " ++ show formula10
    putStrLn $ "Avaliação: " ++ show (evaluate formula10)
    putStrLn $ "Árvore de Tableaux:\n " ++ buildTree formula10
