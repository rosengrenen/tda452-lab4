module Expr where

data Expr = Num Double
  | Add Expr Expr
  | Mul Expr  Expr 
  | Cos Expr
  | Sin Expr
  | Op Expr Operand Expr
  | Func Name Expr
  | Var
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
add expr1 expr2 = Add expr1 expr2
mul expr1 expr2 = Mul expr1 expr2
sin,cos :: Expr -> Expr
sin expr = Sin expr
cos expr = Cos expr