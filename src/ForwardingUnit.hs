module ForwardingUnit where

import Clash.Prelude (Show, Generic, NFDataX, Bool, otherwise, (&&), (/=), (==), TopEntity (..), PortName (..))
import Types.AluSrc as AluSrc (AluSrc)
import qualified Types.AluSrc as AluSrc (AluSrc (..))
import Types.Newtype (Reg (..), Enable (..))
import Types.NonImmAluSrc as NonImmAluSrc (NonImmAluSrc)
import qualified Types.NonImmAluSrc as NonImmAluSrc (NonImmAluSrc (..))

data ForwardingUnitInput = ForwardingUnitInput {
  use_imm :: Bool
  , ex_reg_w_en, mem_reg_w_en :: Enable
  , id_rs1, id_rs2 :: Reg
  , ex_reg_dst, mem_reg_dst :: Reg

} deriving (Show, Generic, NFDataX)

data ForwardingUnitOutput = ForwardingUnitOutput {
  -- | Which stage/input the alu should read from.
  alu_src_a :: NonImmAluSrc
  , alu_src_b :: AluSrc
  -- | Which stage/input the data to write to memory should come from.
  , mem_src :: NonImmAluSrc

} deriving (Show, Generic, NFDataX)

{-# NOINLINE forwardingUnit #-}
{-# ANN forwardingUnit (Synthesize {
  t_name = "ForwardingUnit",
  t_inputs = [PortProduct "" [PortName "i_use_imm", PortName "i_ex_reg_w_en", PortName "i_mem_reg_w_en", PortName "i_id_rs1", PortName "i_id_rs2", PortName "i_ex_reg_dst", PortName "i_mem_reg_dst"]],
  t_output = PortProduct "" [PortName "o_alu_src_a", PortName "o_alu_src_b", PortName "o_mem_src"]
}) #-}
forwardingUnit :: ForwardingUnitInput -> ForwardingUnitOutput
forwardingUnit ForwardingUnitInput {use_imm, ex_reg_w_en=Enable ex_reg_w_en, mem_reg_w_en = Enable mem_reg_w_en, id_rs1, id_rs2, ex_reg_dst, mem_reg_dst} = ForwardingUnitOutput {alu_src_a, alu_src_b, mem_src}
  where
    ex_reg_dst_nonzero = let Reg ex_reg_dst_inner = ex_reg_dst in ex_reg_dst_inner /= 0
    mem_reg_dst_nonzero = let Reg mem_reg_dst_inner = mem_reg_dst in mem_reg_dst_inner /= 0

    alu_src_a
      -- If data is produced one stage ahead in the execute stage.
      -- In other words, instruction source register 1 matches the destination of the data in the ex stage and the data is going to be written into the register file.
      | ex_reg_w_en && (id_rs1 == ex_reg_dst) && ex_reg_dst_nonzero = NonImmAluSrc.EX
      -- If data is produced two stages ahead in the memory stage.
      | mem_reg_w_en && (id_rs1 == mem_reg_dst) && mem_reg_dst_nonzero = NonImmAluSrc.MEM
      -- Data produced in write-back or beyond.
      | otherwise = NonImmAluSrc.REG

    alu_src_b
      -- If immediate instruction don't forward.
      | use_imm = AluSrc.IMM
      -- If data is produced one stage ahead in the execute stage.
      | ex_reg_w_en && (id_rs2 == ex_reg_dst) && ex_reg_dst_nonzero = AluSrc.EX
      -- If data is produced two stages ahead in the memory stage.
      | mem_reg_w_en && (id_rs2 == mem_reg_dst) && mem_reg_dst_nonzero = AluSrc.MEM
      -- Data produced in write-back or beyond.
      | otherwise = AluSrc.REG

    -- Data input to memory comes from reg_b in single-stage design, so the logic here will look the same as reg_b other than not using immediate.
    -- It is up to `mem_w_en` in the instruction decode for whether to use this value or not.
    mem_src
      | ex_reg_w_en && (id_rs2 == ex_reg_dst) && ex_reg_dst_nonzero = NonImmAluSrc.EX
      | mem_reg_w_en && (id_rs2 == mem_reg_dst) && mem_reg_dst_nonzero = NonImmAluSrc.MEM
      | otherwise = NonImmAluSrc.REG
