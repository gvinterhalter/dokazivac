
module Formula (
  Name, Value
  , Term (Var, Const, Fn)
  , Formula (T, F, Atom, Not, And, Or, Imp, Iff, A, E, U)
  ) where

-- Sintaksa logike I reda definisemo u nekoliko faza:
-- 1. Termovi
-- 2. Atomicke formule
-- 3. Formule

-- Termovi su izrazi  izgradjeni primenom funkciskih simbola
-- na konstante i promenljive



type Name = String
type Value = String

data Term = Var Name
          | Const Value 
          | Fn Name [Term]
          deriving (Eq, Ord)

-- data Fixation = Pre | In | Post | Multi deriving(Show, Eq, Ord)


-- Atomicke formule se grade primenom relaciskih simbola nad termove
-- formule se grade od atomickih  formula primenom kvantifikatora
-- i drugih logickih  operacija ( ne, i, ili, ako, akko )

data Formula = T
             | F
             | Atom Name [Term]
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imp Formula Formula 
             | Iff Formula Formula
             | A Name Formula
             | E Name Formula
             | U Name Formula
             deriving (Eq, Ord)


