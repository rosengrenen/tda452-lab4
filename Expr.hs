module Expr where
import Data.Char
import Data.Maybe
import Parsing
import Test.QuickCheck

-- A --------------------------------------------------------------------------

data Expr = Num Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
  | Var
  deriving (Eq, Show)
  
-- type Name = String
-- type Operand = Char

-- operators :: [(Char, (Integer, Double -> Double -> Double))]
-- operators = [
--   ('+', (0, (+))),
--   ('-', (0, (-))),
--   ('*', (1, (*))),
--   ('/', (1, (/))) ]

-- functions :: [(String, Double -> Double)]
-- functions = [
--   ("cos", Prelude.cos),
--   ("sin", Prelude.sin) ]

x :: Expr
x = Var
num :: Double -> Expr
num = Num
add,mul :: Expr -> Expr -> Expr
add = Add
mul = Mul
sin,cos :: Expr -> Expr
sin = Sin
cos = Cos

-- B --------------------------------------------------------------------------

showExpr :: Expr -> String
showExpr Var = "x"
showExpr (Num n) = show n
showExpr (Add e e') = showExpr e ++ " + " ++ showExpr e'
showExpr (Mul e e') = showFactor e ++ " * " ++ showFactor e'
showExpr (Sin e) = "sin" ++ showFunction e
showExpr (Cos e) = "cos" ++ showFunction e

showFactor :: Expr -> String
showFactor e@(Add _ _) = "(" ++ showExpr e ++ ")"
showFactor e           = showExpr e

showFunction :: Expr -> String
showFunction e@(Add _ _) = "(" ++ showExpr e ++ ")"
showFunction e@(Mul _ _) = "(" ++ showExpr e ++ ")"
showFunction e           = " " ++ showExpr e

-- C --------------------------------------------------------------------------

eval :: Expr -> Double -> Double
eval Var        var = var
eval (Num num)  _   = num
eval (Add e e') var = eval e var + eval e' var
eval (Mul e e') var = eval e var * eval e' var
eval (Sin e)    var = Prelude.sin $ eval e var
eval (Cos e)    var = Prelude.cos $ eval e var

-- D --------------------------------------------------------------------------

readExpr :: String -> Maybe Expr
readExpr s = maybeExpr $ parse expr (filter (/=' ') s)

maybeExpr :: Maybe (Expr, String) -> Maybe Expr
maybeExpr (Just (e, s)) | s == ""    = Just {-- $assoc --}e
                         | otherwise = Nothing
maybeExpr Nothing                    = Nothing

number :: Parser Expr
number = Num . head <$> oneOrMore readsP


variable :: Parser Expr
variable = do char 'x'; return Var

expr :: Parser Expr
expr = foldl1 Add <$> chain term (char '+')

term :: Parser Expr
term = foldl1 Mul <$> chain factor (char '*')

factor :: Parser Expr
factor = parentheses <|> func <|> number <|> variable

parseSin :: Parser Expr
parseSin = do
  char 's'
  char 'i'
  char 'n'
  Sin <$> factor

parseCos :: Parser Expr
parseCos = do
  char 'c'
  char 'o'
  char 's'
  Cos <$> factor

func :: Parser Expr
func = parseSin <|> parseCos

parentheses :: Parser Expr
parentheses = char '(' *> expr <* char ')'

-- E --------------------------------------------------------------------------

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = doubleEq 0.0001 (eval (fromJust (readExpr (showExpr e))) 1) (eval e 1)

doubleEq :: Double -> Double -> Double -> Bool
doubleEq tol a b = tol > abs (a - b)

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1, rNum 4),(1, rVar),(s, rBin s), (s, rFunc s)]

rNum :: Double -> Gen Expr
rNum range = {--assoc <$> --}elements (map Num [0..range])

rVar :: Gen Expr
rVar = elements [Var]

rBin :: Int -> Gen Expr
rBin size = do 
  let size' = size `div` 2
  op <- elements [Mul, Add]
  e <- arbExpr size'
  e' <- arbExpr size'
  return $ op e e'

rFunc :: Int -> Gen Expr
rFunc size = do 
  let size' = size `div` 2
  fn <- elements [Sin, Cos]
  e <- arbExpr size'
  return $ fn e

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- F --------------------------------------------------------------------------

simplify :: Expr -> Expr
simplify e | e == simplify' e = e
           | otherwise        = simplify (simplify' e)

simplify' :: Expr -> Expr
simplify' (Add Var       Var       ) = Mul (Num 2) Var
simplify' (Add (Num num) (Num num')) = Num (num + num')
simplify' (Add (Num 0)   e'        ) = e'
simplify' (Add e         (Num 0)   ) = e
simplify' (Add e         e'        ) = Add (simplify e) (simplify e')
simplify' (Mul (Num num) (Num num')) = Num (num * num')
simplify' (Mul (Num 0)   _         ) = Num 0
simplify' (Mul _         (Num 0)   ) = Num 0
simplify' (Mul e         (Num 1)   ) = e
simplify' (Mul (Num 1)   e         ) = e
simplify' (Mul e e'                ) = Mul (simplify e) (simplify e')
simplify' (Sin (Num 0)             ) = Num 0    
simplify' (Sin e                   ) = Sin (simplify e)
simplify' (Cos (Num 0)             ) = Num 1    
simplify' (Cos e                   ) = Cos (simplify e)
simplify' e                          = e

prop_simplify :: Expr -> Bool
prop_simplify e = doubleEq 0.0001 (eval e 1) (eval (simplify e) 1) 

-- G --------------------------------------------------------------------------

differentiate :: Expr -> Expr
differentiate e = simplify (differentiate' (simplify e))

differentiate' :: Expr -> Expr
differentiate' Var        = Num 1
differentiate' (Num _)    = Num 0
differentiate' (Add e e') = Add (differentiate' e) (differentiate' e')
differentiate' (Mul e e') = Add (Mul (differentiate' e) e') (Mul e (differentiate' e'))
differentiate' (Cos e)    = Mul (Num (-1)) (Mul (Sin e) (differentiate' e))
differentiate' (Sin e)    = Mul (Cos e) (differentiate' e)
