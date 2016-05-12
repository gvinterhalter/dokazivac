import Data.Char

data Term = Var String
          | Const String
          | Fn String [Term]
          deriving(Eq)

var (c:cs) = Var (toUpper c : cs)

data Fol = R String [Term] deriving(Eq)

showTerms [t] = show t
showTerms (t:ts) = show t ++ ", " ++ showTerms ts

instance Show Term where
  show (Var x) = x
  show (Const c) = c 
  show (Fn f [t1]) = f ++ show t1
  show (Fn f [t1, t2]) =  "(" ++ show t1 ++ f ++  show t2 ++ ")"
  show (Fn f ts) =  f ++ "(" ++ showTerms ts ++ ")"

instance Show Fol where
  show (R p []) = p
  show (R p [t]) = p ++ show t 
  show (R p [t1, t2]) = "[ " ++ show t1 ++" "++ p ++" "++ show t2 ++ " ]"
  show (R p ts) =  p ++ "[" ++ showTerms ts ++ "]"

t1 = Fn "+" [Var "x" , Const "1"]
t2 = Fn "*" [t1, t1] 
t3 = Fn "-" [t2]

p = R "NIJE" [t1, t3]

t = Fn "f" [Const "0", t1, Var "y"]


apply v t = v t

termval (domain, func, pred) v t =
  case t of
    Var x     -> apply v x
    Fn f args -> func f (map (termval i v )  args) 
  where i = (domain, func, pred)

holds (domain, func, pred) v fm = 
  case fm of
  False    -> false
  True     -> true
  Atom(R(r,args)) -> pred r (map (termval i v) args)
  Not(p) -> not $ holds i v p 
  And(p, q) -> (holds m v p) & (holds m v q)
  Or(p, q) -> (holds m v p) or (holds m v q)
  Imp(p, q) -> not (holds m v p) or (holds m v q)
  Iff(p, q) -> (holds m v p == holds m v q)
  -- Forall(x, p) -> forall ( \ a ->  holds m ((x |-> a) v) p ) domain
  -- Exists(x, p) -> exists ( \ a ->  holds m ((x |-> a) v) p ) domain


bool_interp = 
  func f args = 
    match (f, args) with
    ("0", []) -> false
    ("1", []) -> false


