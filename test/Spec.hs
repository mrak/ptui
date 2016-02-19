import qualified Pt.StateMachineTest as SM

import Test.HUnit
import Control.Monad (void,mapM_)

main :: IO ()
main = void $ mapM_ runTestTT [ SM.tests ]
