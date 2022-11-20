-- #TODO 
-- 1. Criação da estrutura em arvore
-- 2. Manipulação do nós
-- 3. Processamento da entrada
-- 4. Processamento dos operandos

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

op :: Formula -> String
op form = txt(form) -- #TODO varredura do texto em busca do operador

op_dir :: Formula -> Formula 
op_dir form = Formula {txt = txt(form), valor = False} -- #TODO varredura do texto em busca dos operandos

op_esq :: Formula -> Formula 
op_esq form = Formula {txt = txt(form), valor = False} -- #TODO varredura do texto em busca dos operandos

neg :: Formula -> No -> Bool
neg form no
  | op(form) == "∧" = neg (form_no (criar_no (op_esq form) no)) no && neg (form_no (criar_no (op_dir form) no) ) no
  | op(form) == "∨" = neg (op_esq form) no || neg (op_dir form) no
  | op(form) == "→" = valor (op_esq form) && neg (op_dir form) no
  | op(form) == "↔" = (valor (op_esq form) && neg (op_dir form) no) || (neg (op_esq form) no && valor (op_dir form))
  | otherwise = neg(op_esq form) no

avalia :: Formula -> No -> Bool
avalia form no
  | op(form) == "∧" = valor(op_esq(form)) && valor(op_dir(form))
  | op(form) == "∨" = avalia (form_no (criar_no (op_esq form) no)) no || avalia (form_no (criar_no (op_dir form) no)) no
  | op(form) == "→" || op(form) == "↔" = True
  | otherwise = valor (op_esq form)


