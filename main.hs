-- Definição da sintaxe das fórmulas proposicionais
data Formula = Var String         -- Variável proposicional
             | Not Formula        -- Negação
             | And Formula Formula  -- Conjunção
             | Or Formula Formula   -- Disjunção
             | Implies Formula Formula  -- Implicação
             deriving (Eq, Show)

-- Definição dos nós da árvore do tableau
data Node = Truth Formula      -- Nó com fórmula rotulada como verdadeira
          | Falsity Formula    -- Nó com fórmula rotulada como falsa
          deriving (Eq, Show)

-- Função para aplicar as regras
applyRules :: Formula -> [Node] -> [Node]
applyRules formula nodes =
    case formula of
        Var _ -> nodes  -- Não há regras para variáveis
        Not (Var _) -> nodes  -- Não há regras para negações de variáveis
        And f1 f2 -> applyAndRule f1 f2 nodes
        Or f1 f2 -> applyOrRule f1 f2 nodes
        Implies f1 f2 -> applyImpliesRule f1 f2 nodes
        Not f -> applyNotRule f nodes

-- Aplicar regra para a conjunção (AND)
applyAndRule :: Formula -> Formula -> [Node] -> [Node]
applyAndRule f1 f2 nodes =
    let newNodes = map (\node -> case node of
                                    Truth (And a b) -> [Truth a, Truth b, node]
                                    _ -> [node]) nodes
    in concatMap (\n -> applyRules f2 (applyRules f1 [n])) (concat newNodes)

-- Aplicar regra para a disjunção (OR)
applyOrRule :: Formula -> Formula -> [Node] -> [Node]
applyOrRule f1 f2 nodes =
    let newNodes = map (\node -> case node of
                                    Falsity (Or a b) -> [Falsity a, Falsity b, node]
                                    _ -> [node]) nodes
    in concatMap (\n -> applyRules f2 (applyRules f1 [n])) (concat newNodes)

-- Aplicar regra para a implicação (IMPLIES)
applyImpliesRule :: Formula -> Formula -> [Node] -> [Node]
applyImpliesRule f1 f2 nodes =
    let newNodes = map (\node -> case node of
                                    Truth (Implies a b) -> [Falsity a, Truth b, node]
                                    _ -> [node]) nodes
    in concatMap (\n -> applyRules f2 (applyRules f1 [n])) (concat newNodes)

-- Aplicar regra para a negação (NOT)
applyNotRule :: Formula -> [Node] -> [Node]
applyNotRule f nodes =
    let newNodes = map (\node -> case node of
                                    Falsity (Not a) -> [Truth a, node]
                                    _ -> [node]) nodes
    in concatMap (\n -> applyRules f [n]) (concat newNodes)

-- Função auxiliar para remover um nó da lista de nós
removeNode :: Node -> [Node] -> [Node]
removeNode _ [] = []
removeNode node (x:xs) =
    if node == x then xs else x : removeNode node xs

-- Função principal para criar a árvore de prova/refutação
createTableau :: Formula -> [Node]
createTableau formula = createTableau' [Falsity formula]

-- Função auxiliar recursiva para criar a árvore de prova/refutação
createTableau' :: [Node] -> [Node]
createTableau' [] = []
createTableau' nodes =
    let newNodes = foldr (\node acc -> applyRulesForNode node acc) [] nodes
    in if nodes == newNodes then nodes
       else createTableau' newNodes

-- Aplicar regras para um nó específico
applyRulesForNode :: Node -> [Node] -> [Node]
applyRulesForNode node acc =
    case node of
        Truth f -> applyRules f acc
        Falsity f -> applyRules (Not f) acc

-- Função para verificar se a árvore de prova/refutação é fechada (não há contradições)
isClosed :: [Node] -> Bool
isClosed nodes = all (\n -> not (isContradiction n nodes)) nodes

-- Verificar se um nó é uma contradição na lista de nós
isContradiction :: Node -> [Node] -> Bool
isContradiction node nodes =
    case node of
        Truth f -> any (\n -> isContradiction' f n) nodes
        Falsity f -> any (\n -> isContradiction' f n) nodes

-- Verificar se uma fórmula é uma contradição em relação a um nó
isContradiction' :: Formula -> Node -> Bool
isContradiction' formula node =
    case node of
        Truth f -> f == Not formula
        Falsity f -> f == formula

-- Função para imprimir a árvore de prova/refutação
printTableau :: [Node] -> IO ()
printTableau nodes = mapM_ print nodes

-- Exemplo de uso
main :: IO ()
main = do
    let formula = Implies (And (Var "A") (Var "B")) (Var "A")
        tableau = createTableau formula
    putStrLn "Árvore de prova/refutação:"
    printTableau tableau
    putStrLn "Fechada? " 
    print (isClosed tableau)
