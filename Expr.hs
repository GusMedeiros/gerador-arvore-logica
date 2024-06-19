module Expr (Expr(..)) where

data Expr =
    Expr String Bool Bool
    | Empty_expr
    deriving (Show)

