module Node (Node(..)) where

import Expr (Expr)

data Node =
    Node Expr Node Node
    --  EXPR   ESQ   DIR
    | Empty
    deriving (Show)
