module Tests.Alu where

import Alu (aluT)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Types.AluCtrl (AluCtrl (..))
import Types.Newtype (Word (..))
import Prelude hiding (Word)

aluTestCase :: String -> Word -> AluCtrl -> Word -> Word -> TestTree
aluTestCase msg expected ctrl a b =
  testCase msg $
    assertEqual "" expected $
      fst $ aluT ctrl a b

unitTests :: TestTree
unitTests =
  testGroup
    "Alu tests"
    [ -- LUI doesn't do anything in the ALU, immediate generator does, so this is identity.
      -- aluTestCase "LUI" (Word 0b11111000010001001011000000000000) LUI (Word 0b11111000010001001011111111111111) (Word 1),
      aluTestCase "LUI" (Word 1) LUI (Word 0b11111000010001001011111111111111) (Word 1),
      testGroup
        "SLT"
        [ aluTestCase "" (Word 1) SLT (Word (-1)) (Word 1),
          aluTestCase "" (Word 0) SLT (Word 1) (Word 1),
          aluTestCase "" (Word 1) SLT (Word (-2)) (Word 1),
          aluTestCase "" (Word 0) SLT (Word 2) (Word 1)
        ],
      testGroup
        "SLTU"
        [ aluTestCase "" (Word 0) SLTU (Word (-2)) (Word 1),
          aluTestCase "" (Word 0) SLTU (Word 2) (Word 1),
          aluTestCase "" (Word 1) SLTU (Word 0) (Word 1)
        ]
    ]
