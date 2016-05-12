module Folly.Clause(Clause,
                    givenClause, Folly.Clause.empty,
                    deleteTautologies,
                    resolvedClauses,
                    showTrace) where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

import Folly.Formula
import Folly.Unification

data Clause = Clause (Set Formula) Justification
           deriving (Show)

data Justification
  = Given
  | Resolved Clause Clause Unifier Unifier
    deriving (Eq, Ord, Show)

instance Eq Clause where
  (==) (Clause cs1 _) (Clause cs2 _) = cs1 == cs2

instance Ord Clause where
  (<=) (Clause cs1 _) (Clause cs2 _) = cs1 <= cs2

givenClause cs = Clause cs Given
resolvedClause cs lc rc mgu = Clause cs (Resolved lc rc mgu M.empty)

empty = Clause S.empty Given

applySub :: Unifier -> Clause -> Clause
applySub u c@(Clause lits j) =
  Clause (S.map (\lit -> applyToTerms lit (applyUnifier u)) lits) j

allVars (Clause cs _) = L.concatMap collectVars $ S.toList cs

deleteTautologies :: Set Clause -> Set Clause
deleteTautologies formulas = S.filter (not . dnfIsTautology) formulas

dnfIsTautology :: Clause -> Bool
dnfIsTautology (Clause lits _) = isTautology lits

isTautology :: Set Formula -> Bool
isTautology c = S.size (S.intersection atoms negAtoms) > 0
  where
    atoms = S.filter isAtom c
    negAtoms = S.map stripNegations $ S.filter (not . isAtom) c

resolvedClauses :: Clause -> Clause -> Set Clause
resolvedClauses c1 c2 =
  let rho = uniqueVarSub (allVars c1) (allVars c2) in
   resolvedClausesU (applySub rho c1) c2

resolvedClausesU :: Clause -> Clause -> Set Clause
resolvedClausesU l@(Clause left _) r@(Clause right _) =
  case left == right of
   True ->  S.empty
   False -> S.fromList resClauses
  where
    possibleResCombos = [(x, l, y, r) | x <- (S.toList left), y <- (S.toList right)]
    mResClauses = L.map (\(x, l, y, r) -> tryToResolve x l y r) possibleResCombos
    resClauses = L.map fromJust $ L.filter (/= Nothing) mResClauses

tryToResolve :: Formula -> Clause -> Formula -> Clause -> Maybe Clause
tryToResolve leftLiteral leftClause rightLiteral rightClause =
  case matchingLiterals leftLiteral rightLiteral of
    True -> unifiedResolvedClause leftLiteral leftClause rightLiteral rightClause
    False -> Nothing

unifiedResolvedClause :: Formula -> Clause -> Formula -> Clause -> Maybe Clause
unifiedResolvedClause lLit l@(Clause lc _) rLit r@(Clause rc _) =
  case L.length (literalArgs lLit) == L.length (literalArgs rLit) of
   True ->
     case mostGeneralUnifier $ zip (literalArgs lLit) (literalArgs rLit) of
      Just mgu ->
        Just $ mkResolvedClause mgu resolvedLits l r
      Nothing -> Nothing
   False -> Nothing
  where
    resolvedLits = S.union (S.delete lLit lc) (S.delete rLit rc)

mkResolvedClause :: Unifier -> Set Formula -> Clause -> Clause -> Clause
mkResolvedClause mgu lits lc rc =
  resolvedClause (S.map (\lit -> applyToTerms lit (applyUnifier mgu)) lits) lc rc mgu

showTrace c = showTraceRec 0 c

showTraceRec n c@(Clause cs Given) = (ind n) ++ "GIVEN " ++ showLits cs
showTraceRec n c@(Clause cs (Resolved a b u _)) =
  (ind n) ++ showLits cs ++ "  " ++ show u ++ "\n" ++
  showTraceRec (n+1) a ++ "\n" ++
  showTraceRec (n+1) b

ind n = L.concat $ L.replicate n "  "

showLits ls = "{" ++ (L.concat $ L.intersperse " " $ L.map show lits) ++ "}"
  where
    lits = S.toList ls
    n = L.length lits
