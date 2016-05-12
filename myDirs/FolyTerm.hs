
import Control.Monad
import Data.Set as S
import Data.List as L
import Data.Map as M


data Term = Const String
          | Var String
          | Fn String [Term]
          deriving (Eq, Ord)

t1 = Fn "+" [Var "x" , Const "1"]
t2 = Fn "*" [t1, t1] 
t3 = Fn "-" [t2]

showArgs [t] = show t
showArgs (t:ts) = show t ++ ", " ++ showArgs ts

instance Show Term where
  show (Var x) = x
  show (Const c) = c 
  show (Fn f [t1]) = f ++ show t1
  show (Fn f [t1, t2]) =  "(" ++ show t1 ++ f ++  show t2 ++ ")"
  show (Fn f args) =  f ++ "(" ++ showArgs args ++ ")"






-- Konstruktori
var x = Var x
const c = Const c
fn name args = case (take 3 name) == "skl" of
  True -> error $ "skl je reservisano za skolemizaciju"
  False -> Fn name args

-- type

isVar (Var _) = True
isVar _  = False

isFn (Fn _ _) = True
isFn _ = False

isConst (Const _) = True
isConst _ = False

-- geters

getName (Fn n _) = n
getName (Var n)  = n

getFnArgs (Fn _ a) = a

--

appendVarName suffix (Var n) = Var (n ++ suffix)
appendVarName suffix (Fn n args) = Fn n (L.map (appendVarName suffix) args)
appendVarName _ t = t


fvt :: Term -> Set Term
fvt (Const _) = S.empty
fvt (Var n) = S.fromList [(Var n)]
-- fvt (Fn n args) = S.foldl S.union S.empty (S.fromList (L.map fvt args))
-- fvt (Fn n (a:args)) = S.union (fvt a) (S.fromList (L.map fvt args))



subTerm :: Map Term Term  -> Term -> Term
subTerm _ (Const c) = Const c
subTerm sub (Fn n args) = Fn n (L.map (subTerm sub) args)
subTerm sub (Var x) = case M.lookup (Var x) sub of
  Just s -> s
  Nothing -> (Var x)


data Formula = T
             | F
             | P String [Term]
             | N Formula
             | B String Formula Formula
             | Q String Term Formula
             deriving (Eq, Ord)


instance Show Formula where
  show T = "True"
  show F = "False"
  show (P n a) = n ++ "[" ++ show a ++"]"
  show (N (P n a)) = "~" ++ show (P n a) 
  show (N f) = "~(" ++ show f ++ ")"
  show (B op f1 f2) = "("++ show f1 ++ " " ++ show op ++ " " ++ show f2 ++")"
  show (Q q t f) = "("++ q ++ " " ++ show t ++ " . " ++ show t ++")"


applyToTerms :: Formula -> (Term -> Term) -> Formula
applyToTerms (P n args) f = P n $ L.map f args
applyToTerms (N l) f = N (applyToTerms l f)
applyToTerms (B n f1 f2) f = B n (applyToTerms f1 f) (applyToTerms f2 f)
applyToTerms (Q n t l) f = Q n (f t) (applyToTerms l f)



collectVars :: Formula -> [Term]
collectVars (P _ args) = L.concatMap (\t -> if isVar t then [t] else []) args
collectVars (N f) = collectVars f
collectVars (B _ a b) = collectVars a ++ collectVars b
collectVars (Q _ v f) = v : (collectVars f)
collectVars _ = []


-- primena kvantifikatora

te :: Term -> Formula -> Formula
te v@(Var _) f = Q "E" v f
te t _ = error $ "Ne mogu da kvantifikujem preko ne termova " ++ show t

fa :: Term -> Formula -> Formula
fa v@(Var _) f = Q "V" v f
fa t _ = error $ "Ne mogu da kvantifikujem preko ne termova " ++ show t


pr name args = P name args
con f1 f2 = B "&" f1 f2
dis f1 f2 = B "|" f1 f2
imp f1 f2 = B "->" f1 f2
bic f1 f2 = B "<->" f1 f2
neg f = N f

(/\) = con
(\/) = dis


vars :: Formula -> Set Term

freeVars :: Formula -> Set Term

isAtom :: Formula -> Bool
isAtom (P _ _) = True
isAtom _ = False

stripNegations :: Formula -> Formula
stripNegations (N t) = t
stripNegations f = f

literalArgs :: Formula -> [Term]
literalArgs (P _ a) = a
literalArgs (N (P _ a)) = a







































