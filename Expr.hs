module Expr where
import Data.Char
import Data.Maybe
import Parsing
import Test.QuickCheck

-- A --------------------------------------------------------------------------

data Expr = Num Double
  | Operation Operator Expr Expr
  | Function FunctionName Expr
  | Var
  deriving (Eq, Show)

type Operator = String
type FunctionName = String
  
operators :: [(String, (Integer, Double -> Double -> Double))]
operators = [
  ("+", (0, (+))),
  ("*", (1, (*))) ]

functions :: [(String, Double -> Double)]
functions = [
  ("cos", Prelude.cos),
  ("sin", Prelude.sin) ]

x :: Expr
x = Var
num :: Double -> Expr
num = Num
add,mul :: Expr -> Expr -> Expr
add = Operation "+"
mul = Operation "*"
sin,cos :: Expr -> Expr
sin = Function "sin"
cos = Function "cos"

-- B --------------------------------------------------------------------------

showExpr :: Expr -> String
showExpr expr = showExpr' expr (-1)

showExpr' :: Expr -> Integer -> String
showExpr' (Num num)                 _ = show num
showExpr' (Operation operator e e') p
  | p > precedence = "(" ++ showExpr' e precedence ++ " " ++ operator ++ " " ++ showExpr' e' precedence ++ ")"
  | otherwise      = showExpr' e precedence ++ " " ++ operator ++ " " ++ showExpr' e' precedence
  where
    (precedence, _) = fromJust $ lookup operator operators 
showExpr' (Function name e)         _ = name ++ " " ++ showExpr' e 999
showExpr' Var                       _ = "x"

-- C --------------------------------------------------------------------------

eval :: Expr -> Double -> Double
eval (Num num)                 _ = num
eval (Operation operator e e') x = eval e x `op` eval e' x
  where
    (_, op) = fromJust $ lookup operator operators
eval (Function name e)         x = func $ eval e x
  where
    func = fromJust $ lookup name functions
eval Var                       x = x

-- D --------------------------------------------------------------------------

readExpr :: String -> Maybe Expr
readExpr s = maybeExpr $ parse (expr 0) (filter (/=' ') s)

maybeExpr :: Maybe (Expr, String) -> Maybe Expr
maybeExpr (Just (e, s)) | s == ""    = Just e
                         | otherwise = Nothing
maybeExpr Nothing                    = Nothing

number :: Parser Expr
number = Num . head <$> oneOrMore readsP

variable :: Parser Expr
variable = do char 'x'; return Var

string :: String -> Parser String
string str = traverse char str

expr :: Int -> Parser Expr
expr n
  | n < length operators = foldl1 (Operation operator) <$> chain (expr (n + 1)) (string operator)
  | otherwise            = factor
  where
    (operator, _) = operators !! n

factor :: Parser Expr
factor = parentheses <|> func <|> number <|> variable

func' :: Parser String
func' = foldl1 (<|>) [string $ fst f | f <- functions]

func :: Parser Expr
func = do
  name <- func'
  if isJust $ lookup name functions
  then Function name <$> factor
  else failure

parentheses :: Parser Expr
parentheses = char '(' *> expr 0 <* char ')'

-- E --------------------------------------------------------------------------

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = doubleEq 0.0001 (eval (fromJust (readExpr (showExpr e))) 1) (eval e 1)

doubleEq :: Double -> Double -> Double -> Bool
doubleEq tol a b = tol > abs (a - b)

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1, rNum 4),(1, rVar),(s, rBin s), (s, rFunc s)]

rNum :: Double -> Gen Expr
rNum range = elements (map Num [0..range])

rVar :: Gen Expr
rVar = elements [Var]

rBin :: Int -> Gen Expr
rBin size = do 
  let size' = size `div` 2
  op <- elements [Operation $ fst o | o <- operators]
  e <- arbExpr size'
  e' <- arbExpr size'
  return $ op e e'

rFunc :: Int -> Gen Expr
rFunc size = do 
  let size' = size `div` 2
  fn <- elements [Function $ fst f | f <- functions]
  e <- arbExpr size'
  return $ fn e

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- -- F --------------------------------------------------------------------------

simplify :: Expr -> Expr
simplify e | e == simplify' e = e
           | otherwise        = simplify (simplify' e)

simplify' :: Expr -> Expr
simplify' (Operation "+" Var       Var       ) = Operation "*" (Num 2) Var
simplify' (Operation "+" (Num num) (Num num')) = Num (num + num')
simplify' (Operation "+" (Num 0)   e'        ) = e'
simplify' (Operation "+" e         (Num 0)   ) = e
simplify' (Operation "+" e         e'        ) = Operation "+" (simplify e) (simplify e')
simplify' (Operation "*" (Num num) (Num num')) = Num (num * num')
simplify' (Operation "*" (Num 0)   _         ) = Num 0
simplify' (Operation "*" _         (Num 0)   ) = Num 0
simplify' (Operation "*" e         (Num 1)   ) = e
simplify' (Operation "*" (Num 1)   e         ) = e
simplify' (Operation "*" e e'                ) = Operation "*" (simplify e) (simplify e')
simplify' e@(Function _ (Num _)              ) = Num $ eval e 0
simplify' (Function "sin" e                  ) = Function "sin" (simplify e)
simplify' (Function "cos" e                  ) = Function "cos" (simplify e)
simplify' e                                    = e

prop_simplify :: Expr -> Bool
prop_simplify e = doubleEq 0.0001 (eval e 1) (eval (simplify e) 1) 

-- -- G --------------------------------------------------------------------------

differentiate :: Expr -> Expr
differentiate e = simplify (differentiate' (simplify e))

differentiate' :: Expr -> Expr
differentiate' Var        = Num 1
differentiate' (Num _)    = Num 0
differentiate' (Operation "+" e e') = Operation "+" (differentiate' e) (differentiate' e')
differentiate' (Operation "*" e e') = Operation "+" (Operation "*" (differentiate' e) e') (Operation "*" e (differentiate' e'))
differentiate' (Function "cos" e)   = Operation "*" (Num (-1)) (Operation "*" (Function "sin" e) (differentiate' e))
differentiate' (Function "sin" e)   = Operation "*" (Function "cos" e) (differentiate' e)
differentiate' e                    = error "The calculator does not support differentating this expression"
