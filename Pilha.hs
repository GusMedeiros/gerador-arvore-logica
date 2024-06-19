module Pilha (
    Pilha,
    empty,
    isEmpty,
    push,
    pop,
    top
) where

-- Definindo o tipo Expr
data Expr = Expr String Bool
    deriving (Show)

-- Definindo o tipo Pilha
newtype Pilha = Pilha [Expr]
    deriving (Show)

-- Função para criar uma pilha vazia
empty :: Pilha
empty = Pilha []

-- Função para verificar se a pilha está vazia
isEmpty :: Pilha -> Bool
isEmpty (Pilha []) = True
isEmpty _          = False

-- Função para empilhar um elemento
push :: Expr -> Pilha -> Pilha
push x (Pilha xs) = Pilha (x:xs)

-- Função para desempilhar um elemento
pop :: Pilha -> (Expr, Pilha)
pop (Pilha (x:xs)) = (x, Pilha xs)
pop (Pilha [])     = error "Pilha vazia"

-- Função para ver o elemento no topo da pilha
top :: Pilha -> Expr
top (Pilha (x:_)) = x
top (Pilha [])    = error "Pilha vazia"
