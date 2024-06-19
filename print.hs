data Node =
    Node Expr Node Node
    | Empty
    deriving (Show)

data Expr =
    Expr String Bool
    deriving (Show)

--Essa é a função deve ser chamada na main

printaarvore :: Node -> IO ()
printaarvore arvore = putStrLn $ unlines $ printa arvore

--Essa função converte a árvore em uma lista de string
printa :: Node -> [String]
printa arvore = printaHelper "" arvore


printaHelper :: String -> Node -> [String]
printaHelper _ Empty = []
printaHelper prefix (Node (Expr str bool) left right) =
    [prefix ++ str ++ ": " ++ show bool] ++
    printaSubarvore (prefix ++ "├── ") (prefix ++ "│   ") left ++
    printaSubarvore (prefix ++ "└── ") (prefix ++ "    ") right

printaSubarvore :: String -> String -> Node -> [String]
printaSubarvore prefix1 prefix2 arvore =
    case arvore of
        Empty -> []
        Node (Expr str bool) left right ->
            [prefix1 ++ str ++ ": " ++ show bool] ++
            printaHelper (prefix2 ++ "├── ") left ++
            printaHelper (prefix2 ++ "└── ") right

-- Função principal para teste
main :: IO ()
main = do
    let arvore = Node (Expr "(p->(q->p))" False)
                    (Node (Expr "p" True)
                        Empty
                        Empty
                    )
                    (Node (Expr "(q->p)" False)
                        (Node (Expr "q" True) (Node (Expr "p" False) Empty Empty) Empty)
                        Empty
                    )
    printaarvore arvore
