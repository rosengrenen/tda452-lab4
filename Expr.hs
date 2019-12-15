module Expr where
import Data.Char
import Data.Maybe
import Parsing

-- A --------------------------------------------------------------------------

data Expr = Num Double
  | Op Expr Operand Expr
  | Func Name Expr
  | Var
  deriving Show
type Name = String
type Operand = String
data Opr = Opr Char (Double -> Double -> Double)

operators :: [(String, (Integer, (Double -> Double -> Double)))]
operators = [
  ("+", (0, (+))),
  ("*", (1, (*))) ]

functions :: [(String, (Double -> Double))]
functions = [
  ("cos", Prelude.cos),
  ("sin", Prelude.sin) ]

x :: Expr
x = Var
num :: Double -> Expr
num value = Num value
add,mul :: Expr -> Expr -> Expr
add expr1 expr2 = Op expr1 "+" expr2
mul expr1 expr2 = Op expr1 "*" expr2
sin,cos :: Expr -> Expr
sin expr = Func "sin" expr
cos expr = Func "cos" expr

-- B --------------------------------------------------------------------------

showExpr :: Expr -> String
showExpr expr = showExpr' expr (-1)

showExpr' :: Expr -> Integer -> String
showExpr' (Num num)                _ = show num
showExpr' (Op expr1 operand expr2) p 
  | p > precedence = "(" ++ showExpr' expr1 precedence ++ " " ++ operand ++ " " ++ showExpr' expr2 precedence ++ ")"
  | otherwise      = showExpr' expr1 precedence ++ " " ++ operand ++ " " ++ showExpr' expr2 precedence
  where
    (precedence, _) = fromJust $ lookup operand operators 
showExpr' (Func name expr)         _ = name ++ showExpr' expr 999
showExpr' Var                      _ = "x"

-- C --------------------------------------------------------------------------

eval :: Expr -> Double -> Double
eval (Num num)                _ = num
eval (Op expr1 operand expr2) x = eval expr1 x `op` eval expr2 x
  where
    (_, op) = fromJust $ lookup operand operators
eval (Func name expr)         x = func $ eval expr x
  where
    func = fromJust $ lookup name functions
eval Var                      x = x

-- D --------------------------------------------------------------------------

number :: Parser Double
number = head <$> zeroOrMore readsP

operator :: Parser String
operator = undefined 

function :: Parser String
function = undefined

readExpr :: String -> Maybe Expr
readExpr = undefined
