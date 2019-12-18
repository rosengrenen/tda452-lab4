module Expr where
import Data.Char
import Data.Maybe
import Parsing

-- A --------------------------------------------------------------------------

data Expr = Num Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
  | Var
  deriving Show
  
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

number :: Parser Expr
number = Num . head <$> oneOrMore readsP

parseSin :: Parser Expr
parseSin = do
  char 's'
  char 'i'
  char 'n'
  Sin <$> expression

parseCos :: Parser Expr
parseCos = do
  char 'c'
  char 'o'
  char 's'
  Cos <$> expression

function :: Parser Expr
function = parseSin <|> parseCos

variable :: Parser Expr
variable = do char 'x'; return Var

expression :: Parser Expr
expression = foldl1 Add <$> chain term (char '+')

term :: Parser Expr
term = foldl1 Mul <$> chain factor (char '*')

factor :: Parser Expr
factor = number <|> parentheses <|> variable <|> function

parentheses :: Parser Expr
parentheses = char '(' *> expression <* char ')'
