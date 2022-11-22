-- #TODO 
-- 1. Criação da estrutura em arvore
-- 2. Manipulação do nós
-- 3. Processamento da entrada
-- 4. Processamento dos operandos

-- exp = opr*op*opr
-- opr = (exp) || char

data Formula = Formula {
  txt :: String,
  valor :: Bool
}

data No = No {
  form_no :: Formula,
  pai :: No,
  filhos :: [No]
}

criar_no :: Formula -> No -> No
criar_no formula no_pai = No {form_no = formula, pai = no_pai, filhos = []}

e_op :: String -> Bool
e_op c = if c == "∧" || c == "∨" || c == "→" || c == "↔" then True else False

index_op :: Formula -> Int -> Int -> Int
index_op form index nivel 
  | txt(form) == "" || length(txt form) == 1 = 0
  | txt(form) !! index == '(' = index_op form (index+1) (nivel+1)
  | txt(form) !! index == ')' = index_op form (index+1) (nivel-1)
  | nivel == 0 && e_op([txt form !! index]) = index
  | otherwise = index_op form (index+1) nivel

op :: Formula -> String
op form = [txt form !! (index_op form 0 0)]

op_dir :: Formula -> Formula 
op_dir form = Formula {txt = drop ((index_op form 0 0)+1) (txt form), valor = True}

op_esq :: Formula -> Formula 
op_esq form = 
  let operando = take (index_op form 0 0) (txt form)
  in Formula {txt = operando, valor = True}
-- (~(a&d)>(b|c)
-- ~(a&d) -> ~a & ~d -> False
-- ~(b|c) : F

neg :: Formula -> No -> Bool
neg form no
  | op(form) == "∧" = neg (form_no (criar_no (op_esq form) no)) no && neg (form_no (criar_no (op_dir form) no) ) no
  | op(form) == "∨" = neg (op_esq form) no || neg (op_dir form) no
  | op(form) == "→" = valor (op_esq form) && neg (op_dir form) no
  | op(form) == "↔" = (valor (op_esq form) && neg (op_dir form) no) || (neg (op_esq form) no && valor (op_dir form))
  | op(form) == "¬" = neg form no
  | otherwise = False

avalia :: Formula -> No -> Bool
avalia form no
  | op(form) == "∧" = valor(op_esq form) && valor(op_dir form)
  | op(form) == "∨" = avalia (form_no (criar_no (op_esq form) no)) no || avalia (form_no (criar_no (op_dir form) no)) no
  | op(form) == "→" || op form == "↔" = True
  | otherwise = valor (op_esq form)


