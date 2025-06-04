import Test.Tasty
import qualified Tests.Alu
import qualified Tests.Example.Project
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.Example.Project.accumTests,
        Tests.Alu.unitTests
      ]
