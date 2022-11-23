data Formula = Formula { texto :: String
                       , operando_esq :: String
                       , operando_dir :: String
                       , operador :: Char
                       , valor :: Bool
                       }

data No = No { formulas :: [Formula]
             , no_esq :: No
             , no_dir :: No
             , _folha :: Bool
             }

desenvolve :: Formula -> [Formula]
desenvolve form = 
  let valores = valores_de_regra (operador form) (valor form)
  in 
    [ 
      cria_formula (operando_esq form) (head valores), 
      cria_formula (operando_dir form) (last valores) 
    ]

_ramifica :: Char -> Bool -> Bool
_ramifica op vlr = (op == '&' && vlr == False) || ((op == '|' || op == '>') && vlr)

valores_de_regra :: Char -> Bool -> [Bool]
valores_de_regra op vlr
  | (op == '&' || op == '|') = [vlr, vlr]
  | op == '>' = [vlr, not vlr]
  | op == '~' = [not vlr]

_op_valido :: Char -> Bool
_op_valido c = c == '&' || c == '|' || c == '>' || c == '~'

index_op :: String -> Int -> Int -> Int
index_op txt index nivel 
  | length txt == 1 = 0
  | txt !! index == '(' = index_op txt (index + 1) (nivel + 1)
  | txt !! index == ')' = index_op txt (index + 1) (nivel - 1)
  | nivel == 0 && _op_valido (txt !! index) = index
  | otherwise = index_op txt (index + 1) nivel

retirar_parenteses :: String -> String
retirar_parenteses txt = 
  if head txt == '(' && last txt == ')' 
  then tail $ init txt
  else txt

recupera_op :: String -> Char
recupera_op txt = 
  let index = index_op txt 0 0
  in if index == 0 then '_' else txt !! index

op_dir :: String -> String 
op_dir txt = retirar_parenteses $ drop ((index_op txt 0 0) + 1) txt

op_esq :: String -> String  
op_esq txt = retirar_parenteses $ take (index_op txt 0 0) txt

_atomico :: Formula -> Bool
_atomico form = operador form == '_'

cria_formula :: String -> Bool -> Formula
cria_formula txt vlr = 
  Formula {
    texto = txt,
    operando_dir = op_dir txt,
    operando_esq = op_esq txt,
    operador = recupera_op txt,
    valor = vlr
  }

ramifica :: No -> [Formula] -> No
ramifica no forms =
  if _folha no
  then 
    No{
      formulas = formulas no,
      no_esq = No{formulas = [head forms], no_esq = No{}, no_dir = No{}, _folha = True},
      no_dir = No{formulas = [last forms], no_esq = No{}, no_dir = No{}, _folha = True},
      _folha = False
    }
  else
    No{
      formulas = formulas no,
      no_esq = ramifica (no_esq no) forms,
      no_dir = ramifica (no_dir no) forms,
      _folha = _folha no
    }

cria_arvore :: No -> Int -> No
cria_arvore no index
  | _ramifica (operador form) (valor form) = ramifica no (desenvolve form)
  | not $ _ramifica (operador form) (valor form) = 
    cria_arvore No{
                  formulas = (formulas no) ++ desenvolve form,
                  no_esq = no_esq no,
                  no_dir = no_dir no,
                  _folha = _folha no
                }
                (index + 1)
  | index == length (formulas no) = 
    No{
      formulas = formulas no,
      no_esq = cria_arvore (no_esq no) 0,
      no_dir = cria_arvore (no_dir no) 0,
      _folha = _folha no
    }
  | _atomico form = cria_arvore no (index + 1)
  | otherwise = No{}
  where form = (formulas no) !! index

atomicas :: No -> Int -> [Formula] -> [Formula]
atomicas no index forms
  | index == length (formulas no) = forms
  | _atomico $ (formulas no) !! index = atomicas no (index + 1) (forms ++ [(formulas no) !! index])
  | otherwise = atomicas no (index + 1) forms

_opostas :: Formula -> Formula -> Bool
_opostas form1 form2 = operando_esq form1 == operando_esq form2 && valor form1 /= valor form2

compara_forms :: [Formula] -> Int -> Int -> Bool
compara_forms forms i j
  | _opostas (forms !! i) (forms !! j) = True
  | (j + 1) == length forms = compara_forms forms (i + 1) (i + 2)
  | i == length forms = False

valida_arvore :: No -> Bool
valida_arvore no
  | _folha no = compara_forms (atomicas no 0 []) 0 1
  | valida_arvore (no_esq no) = True
  | valida_arvore (no_dir no) = True


main :: IO()
main = do
  putStrLn "Digite a fórmula:"
  entrada <- getLine
  let no_raiz = cria_arvore No{
                  formulas = [cria_formula entrada False], 
                  no_esq = No {}, 
                  no_dir = No {},
                  _folha = True
                } 0
  if valida_arvore no_raiz 
  then putStrLn "Válido" 
  else putStrLn "Inválido"
  