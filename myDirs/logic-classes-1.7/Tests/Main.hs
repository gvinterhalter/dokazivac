import Common (TestProof)
import Data.Logic.Instances.Test (V, Formula, SkAtom, SkTerm)
import System.Exit
import Test.HUnit
import qualified Logic
import qualified Chiou0 as Chiou0
--import qualified Data.Logic.Tests.TPTP as TPTP
import qualified Data

main :: IO ()
main =
    runTestTT (TestList [Logic.tests,
                         Chiou0.tests,
                         -- TPTP.tests,  -- This has a problem in the rendering code - it loops
                         Data.tests formulas proofs]) >>=
    doCounts
    where
      doCounts counts' = exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)
      -- Generate the test data with a particular instantiation of FirstOrderFormula.
      formulas = Data.allFormulas
      proofs = (Data.proofs :: [TestProof Formula SkAtom SkTerm V])
