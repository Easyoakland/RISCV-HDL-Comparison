 -- Don't know why the below deriving on `RegisterFile` has an issue, because attempting to select either mode manually doesn't work. This silences the warning.
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module RegisterFile where

import Clash.Prelude hiding (Word, Enable)
import Types.Newtype

newtype RegisterFile = RegisterFile (Vec 31 Word)
  deriving (Show, Eq, Generic, BitPack, NFDataX, Default)

readRegister :: RegisterFile -> Reg -> Word
readRegister (RegisterFile regFile) (Reg reg) = if reg == 0 then Word 0 else regFile !! (reg-1)

writeRegister :: RegisterFile -> Reg -> Word -> RegisterFile
writeRegister (RegisterFile regFile) (Reg reg) value =
  RegisterFile $ if reg == 0 then regFile else replace (reg-1) value regFile

{-# NOINLINE registersT #-}
{-# ANN registersT (Synthesize {
  t_name = "RegisterFile",
  t_inputs = [PortName "i_register_file", PortName "i_r_reg_a", PortName "i_r_reg_b", PortName "i_reg_w_en", PortName "i_reg_dst", PortName "i_reg_w_data"],
  t_output = PortProduct "" [PortName "o_register_file", PortName "o_r_reg_data_a", PortName "o_r_reg_data_b"]
}) #-}
registersT :: RegisterFile -> Reg -> Reg -> Enable -> Reg -> Word -> (RegisterFile, Word, Word)
registersT register_file r_reg_a r_reg_b (Enable reg_w_en) w_reg_dst reg_w_data = (register_file', r_reg_a_data, r_reg_b_data)
  where
  register_file' = if reg_w_en then writeRegister register_file w_reg_dst reg_w_data else register_file
  -- Forward the write to the reads as needed instead of using the negative edge of the clock to create an implicit forwarding path like in the System Verilog version.
  -- In CLASH, using multiple domains is more tedious since you have to thread them from the topEntity, and you can't easily generate an inverted clock from the input clock.
  fwd_read r_reg = if r_reg == w_reg_dst && r_reg /= Reg 0 then reg_w_data else readRegister register_file r_reg
  r_reg_a_data = fwd_read r_reg_a
  r_reg_b_data = fwd_read r_reg_b
