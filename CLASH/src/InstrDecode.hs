module InstrDecode where

import Clash.Prelude hiding (Enable)
import Types.Newtype
import Types.RegSrc

-- Define the output record type
data InstrDecodeOutput = InstrDecodeOutput
  { use_imm :: Bool,
    jump :: Bool,
    branch :: Bool,
    mem_r_en :: Enable,
    mem_w_en :: Enable,
    reg_src :: RegSrc,
    reg_w_en :: Enable,
    use_rs1 :: Bool,
    use_rs2 :: Bool
  }
  deriving (Show, Eq, Generic, NFDataX)

isSw :: Opcode -> Bool
isSw opcode = opcode == Opcode 0b0100011

-- Instruction decode function
instrDecode :: Opcode -> InstrDecodeOutput
instrDecode (Opcode opcode) = case opcode of
  -- R-type arithmetic instruction (0b0110011)
  0b0110011 ->
    InstrDecodeOutput
      { use_imm = False,
        jump = False,
        branch = False,
        mem_r_en = Enable False,
        mem_w_en = Enable False,
        reg_src = ALU, -- ALU output
        reg_w_en = Enable True, -- reg write
        use_rs1 = True, -- use rs1
        use_rs2 = True -- use rs2
      }
  -- I-type arithmetic instructions (0b0010011)
  0b0010011 ->
    InstrDecodeOutput
      { use_imm = True, -- use immediate
        jump = False,
        branch = False,
        mem_r_en = Enable False,
        mem_w_en = Enable False,
        reg_src = ALU, -- ALU output
        reg_w_en = Enable True, -- reg write
        use_rs1 = True, -- use rs1
        use_rs2 = False
      }
  -- lw (0b0000011)
  0b0000011 ->
    InstrDecodeOutput
      { use_imm = True, -- use immediate
        jump = False,
        branch = False,
        mem_r_en = Enable True, -- mem read
        mem_w_en = Enable False,
        reg_src = MEM, -- data memory
        reg_w_en = Enable True, -- reg write
        use_rs1 = True, -- use rs1
        use_rs2 = False -- use rs2
      }
  -- sw (0b0100011)
  0b0100011 ->
    InstrDecodeOutput
      { use_imm = True, -- use immediate
        jump = False,
        branch = False,
        mem_r_en = Enable False,
        mem_w_en = Enable True, -- mem write
        reg_src = ALU, -- Don't care value, using ALU as default
        reg_w_en = Enable False,
        use_rs1 = True, -- use rs1
        use_rs2 = True -- use rs2
      }
  -- beq (0b1100011)
  0b1100011 ->
    InstrDecodeOutput
      { use_imm = False,
        jump = False,
        branch = True, -- branch
        mem_r_en = Enable False,
        mem_w_en = Enable False,
        reg_src = ALU, -- Don't care value, using ALU as default
        reg_w_en = Enable False,
        use_rs1 = True, -- use rs1
        use_rs2 = True -- use rs2
      }
  -- jal (0b1101111)
  0b1101111 ->
    -- Immediate is source of jump, but doesn't go to the ALU
    -- This would be where PCsrc would be set to the immediate but this module doesn't handle that
    InstrDecodeOutput
      { use_imm = False,
        jump = True,
        branch = False,
        mem_r_en = Enable False,
        mem_w_en = Enable False,
        reg_src = PC4, -- and link
        reg_w_en = Enable True, -- reg write "link"
        use_rs1 = False,
        use_rs2 = False
      }
  -- lui (0b0110111)
  0b0110111 ->
    InstrDecodeOutput
      { use_imm = True, -- use immediate
        jump = False,
        branch = False,
        mem_r_en = Enable False,
        mem_w_en = Enable False,
        reg_src = ALU, -- ALU output
        reg_w_en = Enable True, -- write reg
        use_rs1 = False,
        use_rs2 = False
      }
  -- Unsupported/invalid instructions shouldn't change state
  _ ->
    InstrDecodeOutput
      { use_imm = False, -- Don't care, using False as default
        jump = False, -- Don't jump
        branch = False, -- Don't branch
        mem_r_en = Enable False, -- Don't touch mem
        mem_w_en = Enable False, -- Don't touch mem
        reg_src = ALU, -- Don't care, using ALU as default
        reg_w_en = Enable False, -- Don't write to reg
        use_rs1 = False, -- Not using rs1
        use_rs2 = False -- Not using rs2
      }

data ParsedInstruction = ParsedInstruction {
  opcode :: Opcode
  , rd :: Reg
  , rs1 :: Reg
  , rs2 :: Reg
  , funct3 :: Funct3
  , funct7 :: Funct7
} deriving (Show, Generic, NFDataX)

{-# NOINLINE parseInstruction #-}
{-# ANN parseInstruction (Synthesize {
  t_name = "parseInstruction",
  t_inputs = [PortName "i_instruction"],
  t_output = PortProduct "" [PortName "opcode", PortName "rd", PortName "rs1", PortName "rs2", PortName "funct3", PortName "funct7"]
}) #-}
parseInstruction :: Instruction -> ParsedInstruction
parseInstruction (Instruction instruction) = ParsedInstruction {opcode, rd, funct3, rs1, rs2, funct7}
  where
    opcode = bitCoerce $ slice d6 d0 instruction
    rd = bitCoerce $ slice d11 d7 instruction
    funct3 = bitCoerce $ slice d14 d12 instruction
    rs1 = bitCoerce $ slice d19 d15 instruction
    rs2 = bitCoerce $ slice d24 d20 instruction
    funct7 = bitCoerce $ slice d31 d25 instruction

