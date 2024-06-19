module TabelaRegras (tabelaRegra) where

tabelaRegra :: (String, String, String, Bool) -> (Bool, String, Bool)
tabelaRegra (p, op, q, valor)
  | op == "->" && valor = (False, "|", True)
  | op == "->" && not valor = (True, "&", False)
  | op == "&"  && valor = (True, "&", True)
  | op == "&"  && not valor = (False, "|", False)
  | op == "|"  && valor = (True, "|", True)
  | op == "|"  && not valor = (False, "&", False)
  | op == "~" && valor = (False, "", False)  -- Negação precisa ser ajustada conforme o contexto da tabela
  | op == "~" && not valor = (True, "", True)
  | otherwise = error ("Operador ou valor inválido:>" ++ op ++"<.\n")
