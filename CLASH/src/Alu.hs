module Alu where

import Clash.Prelude hiding (Word)
import Types.AluCtrl (AluCtrl (..))
import Types.Newtype (Word (..))

-- alu :: (HiddenClockResetEnable dom, KnownDomain dom) => Signal dom (AluCtrl, (Word, Word)) -> Signal dom (Word, Bool)
-- alu = mealy (\() i -> ((), aluT i)) ()

{-# NOINLINE aluT #-}
{-# ANN aluT (Synthesize {
  t_name = "ALU",
  t_inputs = [PortName "i_alu_ctrl", PortName "i_alu_a", PortName "i_alu_b"],
  t_output = PortProduct "" [PortName "o_alu_res", PortName "o_equals"]
}) #-}
aluT :: AluCtrl -> Word -> Word -> (Word, Bool)
aluT ctrl (Word a) (Word b) = (alu_res, equals)
  where
    au :: Unsigned 32 = bitCoerce a
    bu :: Unsigned 32 = bitCoerce b
    equals = a == b
    alu_res = Word
      $ case ctrl of
        AND -> a .&. b
        OR -> a .|. b
        ADD -> a + b
        SLL -> bitCoerce $ shiftL au $ bitCoerce $ zeroExtend b
        SRL -> bitCoerce $ shiftR au $ bitCoerce $ zeroExtend b
        SUB -> a - b
        -- SLT
        -- Sets the destination register to 1 if the first source register is less than
        -- the second source register when both are treated as signed numbers, otherwise it sets
        -- the destination register to 0.
        SLT -> zeroExtend $ bitCoerce $ a < b
        XOR -> a `xor` b
        SLTU -> zeroExtend $ bitCoerce $ au < bu
        SRA -> shiftR a $ bitCoerce $ zeroExtend b
        -- Load the upper 20 bits. The intermediate generation unit will output the already shifted and corrected value. The ALU only needs to pass on the already shifted value since the ImmGen doesn't directly connect to `reg_w_data`. Also note the intermediate output goes to input `i_alu_b` not `i_alu_a`.
        LUI -> b
