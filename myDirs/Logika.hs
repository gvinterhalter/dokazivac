-- data Expr = 
--     Var String
--   | Const Int
--   | Add Expr Expr
--   | Mul Expr Expr
--   -- deriving (Show)
--
-- instance Show Expr where
--   show  (Var s) = s
--   show  (Const i) = show i
--   show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
--   show (Mul a b) = show a ++ "*" ++ show b
--
-- simplify1 :: Expr -> Expr
-- simplify1 e = case e of
--   Add (Const 0) x         -> x
--   Add x (Const 0)         -> x
--   Add (Const a) (Const b) -> Const $ a + b
--   Mul x (Const 0)         -> Const 0
--   Mul (Const 0) x         -> Const 0
--   Mul x (Const 1)         -> x
--   Mul (Const 1) x         -> x
--   Mul (Const a) (Const b) -> Const $ a * b
--   _                       -> e
--
--
--
-- simplify e = case e of
--   Add x y -> simplify1 $ Add (simplify x) (simplify y)
--   Mul x y -> simplify1 $ Mul (simplify x) (simplify y)
--   _       -> simplify1 e
--
--
-- e = Add (Mul (Add (Const 1) (Mul (Const 0) (Var "x"))) (Const 3))
--         (Var "x")
-- s = simplify e
--
--
-- main = putStrLn $ case s of
--   Const x -> show x
--   e -> "Could not simplify, so: " ++ show e







import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set sa S









