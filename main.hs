module Main where
import qualified Pilha
import Data.List (isPrefixOf)
import Data.ByteString (split)
import TabelaRegras
data Node =
    Node Expr Node Node
    --   EXPR ESQ  DIR
          | Empty
          deriving (Show)

data Expr =
    Expr String Bool
    | Empty_expr
    deriving(Show)

-- (p|r) (False)
-- r (false)
criaArvoreRefutacao :: Expr -> Node
criaArvoreRefutacao Empty_expr = Empty
criaArvoreRefutacao (Expr expr_str valor) =
    let
        (p_expr_str, op_expr_str, q_expr_str) = decompoeExpressao expr_str
        -- "r", "", ""
        (p_val, op_tabela, q_val) = tabelaRegra (p_expr_str, op_expr_str, q_expr_str, valor)
        -- false, "", false
        p_expr = Expr p_expr_str p_val
        --            "r"        "false"
        q_expr = Expr q_expr_str q_val
        --            ""         "false"
    in
        case op_tabela of
            "&" -> Node (Expr expr_str valor) (Node p_expr (criaArvoreRefutacao q_expr) Empty) Empty
            "|" -> Node (Expr expr_str valor) (criaArvoreRefutacao p_expr) (criaArvoreRefutacao q_expr)
            _   -> if op_expr_str == "~" then
                      Node (Expr expr_str valor) (criaArvoreRefutacao p_expr) Empty
                   else
                      Node (Expr expr_str valor) (criaArvoreRefutacao Empty_expr) (criaArvoreRefutacao Empty_expr)


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
    printaArvoreAux nivel (Node (Expr expr_str valor) esq dir) = do
        putStrLn $ replicate (nivel * 2) ' ' ++ expr_str ++ " (" ++ show valor ++ ")"
        printaArvoreAux (nivel + 1) esq
        printaArvoreAux (nivel + 1) dir


-- ESPECIFICAÇÕES PARA STRING DE ENTRADA:
-- NÃO CONTER ESPAÇOS
-- TODA EXPRESSÃO ESTAR ENTRE PARÊNTESES

main :: IO ()
main = do
    putStrLn "-- ESPECIFICAÇÕES PARA STRING DE ENTRADA:"
    putStrLn "-- NÃO CONTER ESPAÇOS"
    putStrLn "-- TODA EXPRESSÃO ESTAR ENTRE PARÊNTESES"
    putStrLn "Exemplo: (a&b) ou ~(a)"
    putStrLn "Insira sua expressão:"
    input <- getLine
    let 
        expr = Expr input False
    printaArvore (criaArvoreRefutacao expr)

-- ((p|(q&r))->((p|q)&(p|r)))
