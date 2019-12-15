data Expr = Num Double
  | Add Expr Expr
  | Mul Expr Expr 
  | Cos Expr
  | Sin Expr
  | Var

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