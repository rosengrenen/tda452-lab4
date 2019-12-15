module Expr where

data Expr = Num Double
  | Op Expr Operand Expr
  | Func Name Expr
  | Var
  deriving Show
type Name = String
type Operand = Char
data Opr = Opr Char (Double -> Double -> Double)

operators :: [(Char, (Double -> Double -> Double))]
operators = [
  ('+', (+)),
  ('*', (*)) ]

functions :: [(String, (Double -> Double))]
functions = [
  ("cos", Prelude.cos),
  ("sin", Prelude.sin) ]

x :: Expr
x = Var
num :: Double -> Expr
num value = Num value
add,mul :: Expr -> Expr -> Expr
add expr1 expr2 = Op expr1 '+' expr2
mul expr1 expr2 = Op expr1 '*' expr2
sin,cos :: Expr -> Expr
sin expr = Func "sin" expr
cos expr = Func "cos" expr