data Expr = 
    Var String
  | Const Int
  | Add Expr Expr
  | Mul Expr Expr
  | Min Expr Expr
  | Minu Expr 
  -- deriving (Show)


instance Show Expr where
  show  (Var s) = s
  show  (Const i) = show i
  show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Min a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
  show (Minu e ) = "-" ++ show e 
  show (Mul a b) = show a ++ "*" ++ show b

simplify1 :: Expr -> Expr
simplify1 e = case e of
  Min (Const a) (Const b) -> Const $ a - b
  Min (Const 0) x         -> Minu x
  Min x (Const 0)         -> x

  Add (Const a) (Const b) -> Const $ a + b
  Add (Const 0) x         -> x
  Add x (Const 0)         -> x

  Mul (Const a) (Const b) -> Const $ a * b
  Mul x (Const 0)         -> Const 0
  Mul (Const 0) x         -> Const 0
  Mul x (Const 1)         -> x
  Mul (Const 1) x         -> x
  _                       -> e

simplify :: Expr -> Expr
simplify e = case e of
  Add x y -> simplify1 $ Add (simplify x) (simplify y)
  Add x y -> simplify1 $ Add (simplify x) (simplify y)
  Mul x y -> simplify1 $ Mul (simplify x) (simplify y)
  _       -> simplify1 e


e = Add (Mul (Add (Const 1) (Mul (Const 0) (Const 2))) (Const 3))
        (Const 2)
s = simplify e



main = putStrLn $ case s of
  Const x -> show x
  e -> "Could not simplify, so: " ++ show e
