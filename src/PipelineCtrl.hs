module PipelineCtrl where

import Clash.Prelude hiding (Enable)
import Types.Newtype (Reg (..), Enable (..))
import Types.PcSrc (PcSrc(..))

data PipelineCtrlInput = PipelineCtrlInput {
  ex_branch :: Bool, -- to detect a branch in EX
  ex_equals :: Bool, -- to resolve the branch in EX
  id_jump :: Bool, -- to detect a jump in ID
  id_rs1 :: Reg,
  id_rs2 :: Reg,
  use_rs1 :: Bool, -- to detect if rs1 or rs2 is used
  use_rs2 :: Bool, -- to detect if rs1 or rs2 is used
  ex_mem_r_en :: Enable, -- to detect a load in EX
  ex_jump :: Bool, -- to detect a jal in EX
  ex_reg_dst :: Reg
}

data PipelineCtrlOutput = PipelineCtrlOutput {
  pc_w_en :: Enable, -- to control writes to PC
  addr_sel :: PcSrc, -- select where to get next PC
  flush_if_id :: Bool, -- to flush instruction in fetch
  w_en_if_id :: Enable, -- to control writes to IF/ID reg
  flush_id_ex :: Bool -- to flush instruction in decode
}

-- Pipeline control module
pipelineCtrl :: PipelineCtrlInput -> PipelineCtrlOutput
pipelineCtrl PipelineCtrlInput {ex_mem_r_en = Enable ex_mem_r_en, ..} = PipelineCtrlOutput {pc_w_en, addr_sel, flush_if_id, w_en_if_id, flush_id_ex}
    where
      -- Execute stage determined the branch is taken if instruction is branch and registers are equal.
      ex_branch_taken = ex_branch && ex_equals

      -- Potential memory stage data hazard at rs1 if rs1 in use (source is register file) and value is in execute stage.
      rs1_mem_data_hazard = ex_reg_dst == id_rs1 && use_rs1

      -- Potential memory stage data hazard at rs2 if source is not immediate (source is register file) and value is in execute stage.
      rs2_mem_data_hazard = ex_reg_dst == id_rs2 && use_rs2

      -- Potential memory stage data hazard next cycle on either rs1 or rs2. The potential hazard is only realized as an actual hazard if the instruction's value is only generated in the memory stage.
      mem_data_hazard = rs1_mem_data_hazard || rs2_mem_data_hazard

      -- Combinational logic for flush control
      (flush_if_id, w_en_if_id, flush_id_ex, pc_w_en)
        -- Flush instructions in the fetch and decode stages on branch taken in execute stage.
        | ex_branch_taken = (
          True,
          Enable True, -- store the instruction at the target of the branch
          True,
          Enable True
        )
        -- Flush instructions in fetch stage if jump in decode stage.
        | id_jump = (
          True,
          Enable True, -- store the instruction at the target of the jump
          False,
          Enable True
        )
        -- If a memory stage data hazard occurs in the memory stage (dependency on lw instruction or jal), stall the fetch and decode stages *and* don't update the program counter (no jump full slip).
        -- This is called a "slip".
        -- TODO why is jal pc+4 calculated in the memory stage instead of earlier and forwarded/passed regularly to avoid slip?
        | mem_data_hazard && (ex_mem_r_en || ex_jump) = (
          False, -- Don't flush the value on slip, ...
          Enable False, -- ... instead hold it by disabling writes to if/id. The decode stage will process the same instruction next cycle...
          True, -- ... and flush the value in the id/ex register to avoid side-effects of the slipped instruction. The instruction in id_ex is now the same as the instruction in if_id but the copy in the id_ex register has an un-forwardable data dependency and so it shouldn't be run.
          Enable False
        )
        -- Don't flush or slip otherwise.
        | otherwise = (False, Enable True, False, Enable True)

      -- PC source selection logic
      -- Later stages happened first and should override more recent stages which only speculatively executed (branch in ex before jump in id).
      addr_sel
        | ex_branch_taken = BRANCH
        | id_jump = JUMP
        | otherwise = PC4
