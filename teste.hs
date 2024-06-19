module Main where

import Data.List (isPrefixOf)

-- Função separaBinario
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

-- Função main
main :: IO ()
main = do
    putStrLn "Insira sua expressão:"
    expr <- getLine
    let (expr1, op, expr2) = separaBinario expr
    putStrLn $ "Primeira sub-expressão: " ++ expr1
    putStrLn $ "Operador: " ++ op
    putStrLn $ "Segunda sub-expressão: " ++ expr2
