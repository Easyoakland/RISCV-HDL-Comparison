module ImmGen where

import Clash.Explicit.Prelude (Nat, error)
import Clash.Prelude (BitVector, Resize (signExtend), bitCoerce, slice, (!), ($))
import Clash.Promoted.Nat.Literals as Nat
import Types.Newtype (Instruction (..), Opcode (..), Word (..))

immGen ::
  -- instruction to decode immediate from
  Instruction ->
  -- immediate is sign extended
  Word
immGen (Instruction instruction) = immediate
  where
    (Opcode opcode) :: Opcode = Opcode $ bitCoerce $ slice Nat.d6 Nat.d0 instruction
    immediate = Word $ bitCoerce $
      case opcode of
        -- I-type arithmetic instructions
        0b0010011 -> signExtend $ slice Nat.d31 Nat.d20 instruction
        -- lw
        0b0000011 -> signExtend $ slice Nat.d31 Nat.d20 instruction
        -- sw
        0b0100011 -> signExtend $ bitCoerce (slice Nat.d31 Nat.d25 instruction, slice Nat.d11 Nat.d7 instruction)
        -- lui
        0b0110111 -> signExtend $ bitCoerce (slice Nat.d31 Nat.d12 instruction, 0 :: BitVector 12)
        -- beq
        0b1100011 -> signExtend $ bitCoerce (instruction ! (31 :: Nat), instruction ! (7 :: Nat), slice Nat.d30 Nat.d25 instruction, slice Nat.d11 Nat.d8 instruction, 0 :: BitVector 1)
        -- jal
        0b1101111 -> signExtend $ bitCoerce (instruction ! (31 :: Nat), slice Nat.d19 d12 instruction, instruction ! (20 :: Nat), slice Nat.d30 Nat.d21 instruction, 0 :: BitVector 1)
        _ -> error "invalid opcode"
