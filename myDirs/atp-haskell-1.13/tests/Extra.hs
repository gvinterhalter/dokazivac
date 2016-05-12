{-# LANGUAGE GADTs, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}
module Extra where

import Data.List as List (map)
import Data.Logic.ATP.Apply (pApp)
import Data.Logic.ATP.Equate ((.=.))
import Data.Logic.ATP.Formulas
import Data.Logic.ATP.Lib (Depth(Depth), Failing(Failure, Success))
import Data.Logic.ATP.Lit ((.~.))
import Data.Logic.ATP.Meson (meson)
import Data.Logic.ATP.Pretty (prettyShow, testEquals)
import Data.Logic.ATP.Prop hiding (nnf)
import Data.Logic.ATP.Quantified (for_all, exists)
import Data.Logic.ATP.Parser (fof)
import Data.Logic.ATP.Resolution
import Data.Logic.ATP.Skolem (Formula, HasSkolem(toSkolem), skolemize, runSkolem, SkAtom, SkTerm)
import Data.Logic.ATP.Tableaux (K(K), tab)
import Data.Logic.ATP.Term (vt, fApp)
import Data.Map as Map (empty)
import Data.Set as Set (fromList, minView, null, Set, singleton)
import Data.String (fromString)
import Test.HUnit

testExtra :: Test
testExtra = TestList [test01, test02, test05, test06, test07]

test05 :: Test
test05 = TestLabel "Socrates syllogism" $ TestCase $ assertEqual "Socrates syllogism" expected input
    where input = (runSkolem (resolution1 socrates),
                   runSkolem (resolution2 socrates),
                   runSkolem (resolution3 socrates),
                   runSkolem (presolution socrates),
                   runSkolem (resolution1 notSocrates),
                   runSkolem (resolution2 notSocrates),
                   runSkolem (resolution3 notSocrates),
                   runSkolem (presolution notSocrates))
          expected = (Set.singleton (Success True),
                      Set.singleton (Success True),
                      Set.singleton (Success True),
                      Set.singleton (Success True),
                      Set.singleton (Success {-False-} True),
                      Set.singleton (Success {-False-} True),
                      Set.singleton (Failure ["No proof found"]),
                      Set.singleton (Success {-False-} True))

socrates :: Formula
socrates =
    (for_all x (s [vt x] .=>. h [vt x]) .&. for_all x (h [vt x] .=>. m [vt x])) .=>. for_all x (s [vt x] .=>. m [vt x])
    where
      x = fromString "x"
      s = pApp (fromString "S")
      h = pApp (fromString "H")
      m = pApp (fromString "M")

notSocrates :: Formula
notSocrates =
    (for_all x (s [vt x] .=>. h [vt x]) .&. for_all x (h [vt x] .=>. m [vt x])) .=>. for_all x (s [vt x] .=>.  ((.~.)(m [vt x])))
    where
      x = fromString "x"
      s = pApp (fromString "S")
      h = pApp (fromString "H")
      m = pApp (fromString "M")

test06 :: Test
test06 =
    let fm :: Formula
        fm = for_all "x" (vt "x" .=. vt "x") .=>. for_all "x" (exists "y" (vt "x" .=. vt "y"))
        expected :: PFormula SkAtom
        expected =  (vt "x" .=. vt "x") .&. (.~.) (fApp (toSkolem "x" 1) [] .=. vt "x")
        -- atoms = [applyPredicate equals [(vt ("x" :: V)) (vt "x")] {-, (fApp (toSkolem "x" 1)[]) .=. (vt "x")-}] :: [SkAtom]
        sk = runSkolem (skolemize id ((.~.) fm)) :: PFormula SkAtom
        table = truthTable sk :: TruthTable SkAtom in
    TestLabel "∀x. x = x ⇒ ∀x. ∃y. x = y" $ TestCase $ assertEqual "∀x. x = x ⇒ ∀x. ∃y. x = y"
                           (expected,
                            TruthTable
                              (List.map asAtom ([vt "x" .=. vt "x", fApp (toSkolem "x" 1) [] .=. vt "x"] :: [Formula]))
                              [([False,False],False),
                               ([False,True],False),
                               ([True,False],True),
                               ([True,True],False)] :: TruthTable SkAtom,
                           Set.fromList [Success (Depth 1)])
                           (sk, table, runSkolem (meson Nothing fm))

asAtom :: forall formula. IsFormula formula => formula -> AtomOf formula
asAtom fm = case Set.minView (atom_union singleton fm :: Set (AtomOf formula)) of
              Just (a, s) | Set.null s -> a
              _ -> error "asAtom"

mesonTest :: (String, Formula, Set (Failing Depth)) -> Test
mesonTest (label, fm, expected) =
    let me = runSkolem (meson (Just (Depth 1000)) fm) in
    TestLabel label $ TestCase $ assertEqual ("MESON test: " ++ prettyShow fm) expected me

fms :: [(String, Formula, Set (Failing Depth))]
fms = [ let [x, y] = [vt "x", vt "y"] :: [SkTerm] in
        ("if x every x equals itself then there is always some y that equals x",
         for_all "x" (x .=. x) .=>. for_all "x" (exists "y" (x .=. y)),
         Set.fromList [Success (Depth 1)]),
        let x = vt "x" :: SkTerm
            [s, h, m] = [pApp "S", pApp "H", pApp "M"] :: [[SkTerm] -> Formula] in
        ("Socrates is a human, all humans are mortal, therefore socrates is mortal",
         (for_all "x" (s [x] .=>. h [x]) .&. for_all "x" (h [x] .=>. m [x])) .=>. for_all "x" (s [x] .=>. m [x]),
         Set.fromList [Success (Depth 3)]) ]

test07 :: Test
test07 = TestList (List.map mesonTest fms)

test01 :: Test
test01 = let fm1 = [fof| ∀a. ¬(P(a)∧(∀y. (∀z. Q(y)∨R(z))∧¬P(a))) |] :: Formula in
         $(testEquals "MESON 1") ([fof| ∀a. ¬(P(a)∧(∀y. (∀z. Q(y)∨R(z))∧¬P(a))) |], Success ((K 2, Map.empty),Depth 2))
              (fm1, tab Nothing fm1)
test02 :: Test
test02 = let fm2 = [fof| ∀a. ¬(P(a)∧¬P(a)∧(∀y z. Q(y)∨R(z))) |] :: Formula in
         $(testEquals "MESON 2") ([fof| ∀a. ¬(P(a)∧¬P(a)∧(∀y z. Q(y)∨R(z))) |], Success ((K 0, Map.empty),Depth 0))
              (fm2, tab Nothing fm2)
{-
i = for_all "a" ((.~.)(p[a] .&. (for_all "y" (for_all "z" (q[y] .|. r[z]) .&. (.~.)(p[a])))))

a = (for_all "a" ((.~.) (((pApp (fromString "P")["a"]) .&. (for_all "y" (for_all "z"
                                                                         (((pApp (fromString "Q")["y"]) .|.
                                                                           (pApp (fromString "R")["z"])) .&.
                                                                          ((.~.) ((pApp (fromString "P")["a"]))))))))))
b = (for_all "a" ((.~.) (((pApp (fromString "P")["a"]) .&. (for_all "y" ((for_all "z"
                                                                          ((pApp (fromString "Q")["y"]) .|.
                                                                           (pApp (fromString "R")["z"]))) .&.
                                                                         ((.~.) ((pApp (fromString "P")["a"])))))))))
-}
{-
test12 :: Test
test12 =
    let fm = (let (x, y) = (vt "x" :: Term, vt "y" :: Term) in ((for_all "x" ((x .=. x))) .=>. (for_all "x" (exists "y" ((x .=. y))))) :: Formula FOL) in
    TestCase $ assertEqual "∀x. x = x ⇒ ∀x. ∃y. x = y" (holds fm) True
-}
