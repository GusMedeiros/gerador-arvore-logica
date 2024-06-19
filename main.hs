module Main where
import Data.List (isPrefixOf)
import TabelaRegras

data Node = Node Expr Node Node
          | Empty
          deriving (Show)

data Expr = Expr String Bool 
          | Empty_expr
          deriving(Show)

data Queue a = Queue [a] [a] deriving Show

enqueue :: Queue a -> a -> Queue a
enqueue (Queue inList outList) x = Queue inList (x:outList)

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue [] []) = (Nothing, Queue [] [])
dequeue (Queue inList (x:xs)) = (Just x, Queue inList xs)
dequeue (Queue inList []) = dequeue (Queue [] (reverse inList))

criaArvoreRefutacao :: Queue Expr -> IO Node
criaArvoreRefutacao queue = do
    putStrLn $ "Queue: " ++ show queue
    case dequeue queue of
        (Nothing, _) -> return Empty
        (Just Empty_expr, queue') -> return Empty
        (Just (Expr expr_str valor), queue') -> do
            putStrLn $ "Processando expressão: " ++ expr_str
            let (p_expr_str, op_expr_str, q_expr_str) = decompoeExpressao expr_str
            let (p_val, op_tabela, q_val) = tabelaRegra (p_expr_str, op_expr_str, q_expr_str, valor)
            let p_expr = Expr p_expr_str p_val
            let q_expr = Expr q_expr_str q_val
            let queueQ = enqueue queue' q_expr
            let queueP = enqueue queue' p_expr
            let queueQP = enqueue (enqueue queue' p_expr) q_expr
            case op_tabela of
                "&" -> do
                    putStrLn "Operador: &"
                    left <- criaArvoreRefutacao queueQP
                    return $ Node (Expr expr_str valor) left Empty
                "|" -> do
                    putStrLn "Operador: |"
                    left <- criaArvoreRefutacao queueP
                    right <- criaArvoreRefutacao queueQ
                    return $ Node (Expr expr_str valor) left right
                _ -> if op_expr_str == "~" then do
                        putStrLn "Operador: ~"
                        left <- criaArvoreRefutacao queueP
                        return $ Node (Expr expr_str valor) left Empty
                     else do
                        left <- criaArvoreRefutacao queue'
                        return $ Node (Expr expr_str valor) left Empty


separaBinario :: String -> (String, String, String)
separaBinario str = splitHelper 0 "" "" (init (tail str))
  where
    operators = ["->", "&", "|"]

    splitHelper :: Int -> String -> String -> String -> (String, String, String)
    splitHelper _ acc1 acc2 [] = (acc1, acc2, "")
    splitHelper counter acc1 acc2 (x:xs)
        | counter == 0 && any (`isPrefixOf` (x:xs)) operators =
            let (op, rest) = findOperator (x:xs)
            in (acc1, op, rest)
        | x == '(' = splitHelper (counter + 1) (acc1 ++ [x]) acc2 xs
        | x == ')' = splitHelper (counter - 1) (acc1 ++ [x]) acc2 xs
        | otherwise = splitHelper counter (acc1 ++ [x]) acc2 xs

    findOperator :: String -> (String, String)
    findOperator s = head [(op, drop (length op) s) | op <- operators, op `isPrefixOf` s]

decompoeExpressao :: String -> (String, String, String)
decompoeExpressao expr
    | head expr == '~' = (drop 2 (init expr), "", "")
    | head expr == '(' = separaBinario expr
    | length expr == 1 = (expr, "", "")
    | otherwise = error "Expressão inválida"


printaarvore :: Node -> IO ()
printaarvore arvore = putStrLn $ unlines $ printa arvore

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

main :: IO ()
main = do
    putStrLn "-- ESPECIFICAÇÕES PARA STRING DE ENTRADA:"
    putStrLn "-- NÃO CONTER ESPAÇOS"
    putStrLn "-- TODA EXPRESSÃO ESTAR ENTRE PARÊNTESES"
    putStrLn "Exemplo: (a&b) ou ~(a)"
    putStrLn "Insira sua expressão:"
    input <- getLine
    let expr = Expr input False
    let initialQueue = enqueue (Queue [] []) expr
    arvore <- criaArvoreRefutacao initialQueue
    printaarvore arvore
    

-- ((p|(q&r))->((p|q)&(p|r)))
