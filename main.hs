module Main where
import Pilha(Pilha)
import qualified Pilha
import Node
import Expr
import Data.List (isPrefixOf)
import Data.ByteString (split)
import TabelaRegras

-- (p|r) (False)
-- r (false)

printaarvore :: Node -> IO ()
printaarvore arvore = putStrLn $ unlines $ printa arvore


--Essa função converte a árvore em uma lista de string
printa :: Node -> [String]
printa arvore = printaHelper "" arvore


--Essa função percorre a string dos filhos e adiciona os traços para exibição
printaHelper :: String -> Node -> [String]
printaHelper _ Empty = []
printaHelper prefix (Node (Expr str bool presente) left right) =
    [prefix ++ str ++ ": " ++ show bool] ++
    printaSubarvore (prefix ++ "├── ") (prefix ++ "│   ") left ++
    printaSubarvore (prefix ++ "└── ") (prefix ++ "    ") right


printaSubarvore :: String -> String -> Node -> [String]
printaSubarvore prefix1 prefix2 arvore =
    case arvore of
        Empty -> []
        Node (Expr str bool presente) left right ->
            [prefix1 ++ str ++ ": " ++ show bool] ++
            printaHelper (prefix2 ++ "├── ") left ++
            printaHelper (prefix2 ++ "└── ") right

criaArvoreRefutacao :: Pilha.Pilha -> Node
criaArvoreRefutacao pilha
    | Pilha.isEmpty pilha = Empty
    | otherwise =
        -- 1((p|(q&r)), 0((p|q)&(p|r)))

        let
            (Expr expr_str valor presente, novaPilha) = Pilha.pop pilha
            (p_expr_str, op_expr_str, q_expr_str) = decompoeExpressao expr_str
            (p_val, op_tabela, q_val) = tabelaRegra (p_expr_str, op_expr_str, q_expr_str, valor)
            p_expr = Expr p_expr_str p_val False
            q_expr = Expr q_expr_str q_val False
        in
            case op_tabela of
                "&" -> Node (Expr expr_str valor False) (Node p_expr (criaArvoreRefutacao (Pilha.push q_expr (pushFilhosP p_expr novaPilha))) Empty) Empty
                "|" -> Node (Expr expr_str valor False) (criaArvoreRefutacao (Pilha.push p_expr novaPilha)) (criaArvoreRefutacao (Pilha.push q_expr novaPilha))
                _   -> if op_expr_str == "~" then
                          Node (Expr expr_str valor False) (criaArvoreRefutacao (Pilha.push p_expr novaPilha)) Empty
                       else
                            let 
                                (Expr a b c, _) = Pilha.pop(novaPilha)
                            in 
                                if c then
                                    let 
                                        (aux, pilha_q) = Pilha.pop(novaPilha)
                                        (_, pilha_intermediaria) = Pilha.pop(pilha_q)
                                        pilha_p = Pilha.push aux pilha_intermediaria
                                    in Node (Expr expr_str valor False) (criaArvoreRefutacao pilha_p) (criaArvoreRefutacao pilha_q)
                                else
                                    Node (Expr expr_str valor False) (criaArvoreRefutacao novaPilha) (Empty)

                        
pushFilhosP :: Expr -> Pilha.Pilha -> Pilha.Pilha
pushFilhosP expr pilha =  
    let
        (Expr expr_str valor presente) = expr
        (p_expr_str, op_expr_str, q_expr_str) = decompoeExpressao expr_str
        (p_val, op_tabela, q_val) = tabelaRegra (p_expr_str, op_expr_str, q_expr_str, valor)
        p_expr = Expr p_expr_str p_val (op_tabela == "|")
        q_expr = Expr q_expr_str q_val (op_tabela == "|")
        pilhaComP = if p_expr_str /= "" && p_expr_str /= expr_str then Pilha.push p_expr pilha else pilha
        pilhaFinal = if q_expr_str /= "" && q_expr_str /= expr_str then Pilha.push q_expr pilhaComP else pilhaComP
        
    in pilhaFinal


mergulhaNot :: String -> String
mergulhaNot str
  | take 2 str == "~(" && last str == ')' = drop 2 (init str)
  | otherwise = error "String não está no formato ~(expr)"

separaBinario :: String -> (String, String, String)
separaBinario str = splitHelper 0 "" "" (init (tail str))
  where
    operators = ["->", "&", "|"]

    splitHelper :: Int -> String -> String -> String -> (String, String, String)
    splitHelper counter acc1 acc2 [] = (acc1, acc2, "")
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
    | head expr == '~' = (drop 2 (init expr), "~", "")
    | head expr == '(' = separaBinario expr
    | all (`elem` ['p'..'z'] ++ ['0'..'9']) expr = (expr, "", "")
    | otherwise = error "Expressão inválida"
    



printaArvore :: Node -> IO ()
printaArvore = printaArvoreAux 0
  where
    printaArvoreAux _ Empty = return ()
    printaArvoreAux nivel (Node (Expr expr_str valor presente) esq dir) = do
        putStrLn $ replicate (nivel * 2) ' ' ++ expr_str ++ " (" ++ show valor ++ ")"
        printaArvoreAux (nivel + 1) esq
        printaArvoreAux (nivel + 1) dir


main :: IO ()
main = do
    putStrLn "-- ESPECIFICAÇÕES PARA STRING DE ENTRADA:"
    putStrLn "-- NÃO CONTER ESPAÇOS"
    putStrLn "-- TODA EXPRESSÃO ESTAR ENTRE PARÊNTESES"
    putStrLn "Exemplo: (a&b) ou ~(a)"
    putStrLn "Insira sua expressão:"
    input <- getLine
    let 
        expr = Expr input False False
    printaarvore (criaArvoreRefutacao (Pilha.push expr Pilha.empty))

-- ((p|(q&r))->((p|q)&(p|r)))
