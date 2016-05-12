
import Formula
import Prelude hiding ( putStr, (+), (-), (*), (/), negate, or, (^), and)
import Data.ByteString.Char8 (putStr)

colBold      str = "\27[1m"  ++ str ++ "\27[22m"
colDim       str = "\27[2m"  ++ str ++ "\27[22m"
colItalic    str = "\27[3m"  ++ str ++ "\27[23m"
colUnderline str = "\27[4m"  ++ str ++ "\27[24m"
colBlink     str = "\27[5m"  ++ str ++ "\27[25m"
colReverse   str = "\27[6m"  ++ str ++ "\27[26m"


colRed       str = "\27[31m" ++ str ++ "\27[97m"
colGreen     str = "\27[32m" ++ str ++ "\27[97m"
colYellow    str = "\27[33m" ++ str ++ "\27[97m"
colBlue      str = "\27[34m" ++ str ++ "\27[97m"
colMagenta   str = "\27[35m" ++ str ++ "\27[97m"
colCyan      str = "\27[36m" ++ str ++ "\27[97m"

cpred  = colGreen
catom  = colBlue
cvar   = colBold . colItalic 
cconst = colDim
cfunc = colMagenta


debug = False

getPres (Var n)   = 9
getPres (Const v) = 9
getPres (Fn n [_]) = 8
getPres (Fn n [_,_]) | n == "-" = 5
                           | n == "+" = 5
                           | n == "*" = 6
                           | n == "/" = 6
                           | otherwise = 7
getPres (Fn _ _) = 8


showArgs [t] = show t
showArgs (t:ts) = show t ++ ", " ++ showArgs ts

instance Show Term where
  show (Var n) =  cvar n
  show (Const v) = cconst v

  show (Fn n [ v@(Const _) ]) = cfunc n ++  show v
  show (Fn n [ c@(Var _) ]) = cfunc n ++  show c
  show (Fn n [ t ]) = cfunc n ++ "(" ++ show t ++")"

  show f@(Fn n [t1, t2]) = (showArg t1) ++ cfunc n  ++ (showArg t2)
    where showArg t 
            | debug || getPres f > getPres t = "(" ++ show t ++ ")"
            | otherwise = show t

  show (Fn n args) =  cfunc n ++ "(" ++ showArgs args ++ ")"



instance Show Formula where
  show T =  catom "T"
  show F =  catom "F"
  show (Atom n []) = catom n 
  show (Atom n [t]) = catom n ++ show t 
  show (Atom n [t1,t2]) = 
    catom  "[" ++ show t1 ++ " "++ catom n++" " ++ show t2  ++ catom "]"
  show (Atom n t) = catom (n ++ "[") ++ showArgs t ++ catom "]"

  show (Not (F)) = cpred "not" ++ show F 
  show (Not (T)) = cpred "not" ++ show T 
  show (Not a@(Atom _ _)) = cpred "not" ++ show a
  show (Not f) = cpred "not" ++ "(" ++ show f ++")"

  show (A n f) = cpred "∀" ++ cvar n ++ "." ++ show f
  show (E n f) = cpred "∃" ++ cvar n ++ "." ++ show f
  show (U n f) = cpred "∃!" ++ cvar n ++ "." ++ show f

  show (And f1 f2) = "(" ++ show f1 ++ cpred " and " ++ show f2 ++")" 
  show (Or f1 f2)  = "(" ++ show f1 ++ cpred " or " ++ show f2 ++")" 
  show (Imp f1 f2) = "(" ++ show f1 ++ cpred " imp "  ++ show f2 ++")" 
  show (Iff f1 f2) = "(" ++ show f1 ++ cpred " iff " ++ show f2 ++")" 

-- konstruktori za Const
c :: Show a => a -> Term
c a = Const $ show a

c0 = Const "0"
c1 = Const "1"

-- predefinisane neke promenlojive
x = Var "x"
y = Var "y"
z = Var "z"
m = Var "m"
n = Var "n"
k = Var "k"

-- konstruktor za atome

f :: Name -> Formula
f n = Atom  n []

-- predefinisani neki atomi
aa = f "a"
ab = f "b"
ac = f "c"


infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /
infixl 8 %
infixl 9 ^
negate x = Fn "-" [x] 
x - y = Fn "-" [x, y] 
x + y = Fn "+" [x, y]
x * y = Fn "*" [x, y]
x / y = Fn "/" [x, y]
x % y = Fn "%" [x, y]
x ^ y = Fn "^" [x, y]


infixl 4 `lt`
infixl 4 `gt`
infixl 4 `div`
infixl 5 `eq`

t1 `eq` t2 = Atom "=" [t1, t2]
t1 `lt` t2 = Atom "<" [t1, t2]
t1 `gt` t2 = Atom ">" [t1, t2]
t1 `div` t2 = Atom "|" [t1, t2]


infixl 3 `and`
infixl 2 `or`
infixl 1 `imp`
infixl 0 `iff`

f1 `and` f2 = And f1 f2
f1 `and` f2 = And f1 f2
f1 `or`  f2 = Or  f1 f2
f1 `imp` f2 = Imp f1 f2
f1 `iff` f2 = Iff f1 f2

e = A "a"$ U "b" $ Not (x^y`eq`c 4`and `aa `iff` ac `or`  ab `imp` ab)





