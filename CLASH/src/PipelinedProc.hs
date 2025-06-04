{-
This file describes the basic 5-stage pipelined processor discussed in David Patterson and John Hennessay's Compute rOrganization and Design textbook.
Instruction processing goes through the following stages:

IF - Instruction Fetch
  In this stage instructions are fetched, one per clock cycle, from the memory pointed to by the program counter (PC)
ID - Instruction Decode
  The instruction fetched in the previous stage is decoded into a number of control signals that flow down the pipeline.
  The register file is read in this stage.
  Data and control dependencies are detected and handled in this stage by generating forward or stall/interlock control signals.
EX - Execute
  The arithmetic-logic unit (ALU) is in this stage.
  Branches (such as `beq`) are resolved in this stage.
MEM - Memory
  In this stage memory is accessed. Loads such as `lw` read the memory and stores such as `sw` write to the memory.
WB - Write Back
  The register file is written with the result of the instruction.

This file attempts to follow the order of the pipeline. PC logic is near the beginning and writeback logic is near the end.

The various stage record types correspond to the start of each pipeline stage.
For example id_ex corresponds to EXStage.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

module PipelinedProc where

import Clash.Prelude hiding (Word, Enable)
import Types.AluSrc as AluSrc
import Types.Newtype as Newtype
import Types.NonImmAluSrc as NonImmAluSrc
import Types.PcSrc as PcSrc
import Types.RegSrc as RegSrc
import ImmGen
import Control.Monad.State
import PipelineCtrl (pipelineCtrl, PipelineCtrlInput (..), PipelineCtrlOutput (..))
import InstrDecode (instrDecode, InstrDecodeOutput (..), ParsedInstruction (..), parseInstruction, isSw)
import Alu (aluT)
import AluDecode (aluDecodeT)
import ForwardingUnit (forwardingUnit, ForwardingUnitInput (..), ForwardingUnitOutput (..))
import RegisterFile (RegisterFile (..), registersT)
import Data.Maybe (fromMaybe)


-- | Registers at the beginning of the IF stage
newtype IFStage = IFStage
  { pc :: Ptr
  } deriving (Show, Generic, NFDataX, Default)

-- | Registers at the beginning of the ID stage
data IDStage = IDStage
  { pc :: Ptr
  , instruction :: Instruction
  } deriving (Show, Generic, NFDataX, Default)

-- | Registers at the beginning of the EX stage
data EXStage = EXStage
  { pc :: Ptr
  , opcode :: Opcode
  , funct3 :: Funct3
  , funct7 :: Funct7
  , immediate :: Word
  , alu_src_a :: NonImmAluSrc
  , alu_src_b :: AluSrc
  , reg_data_a :: Word
  , reg_data_b :: Word
  , mem_r_en :: Enable
  , mem_w_en :: Enable
  , mem_src :: NonImmAluSrc
  , reg_w_en :: Enable
  , branch :: Bool
  , jump :: Bool
  , branch_target :: Ptr
  , reg_dst :: Reg
  , reg_src :: RegSrc
  } deriving (Show, Generic, NFDataX, Default)

-- | Registers at the beginning of the MEM stage
data MEMStage = MEMStage
  { pc :: Ptr
  , reg_w_en :: Enable
  , mem_w_en :: Enable
  , mem_r_en :: Enable
  , mem_w_data :: Word
  , alu_res :: Word
  , reg_dst :: Reg
  , reg_src :: RegSrc
  } deriving (Show, Generic, NFDataX, Default)

-- | Registers at the beginning of the WB stage
data WBStage = WBStage
  { pc_plus_4 :: Ptr
  , reg_w_en :: Enable
  , mem_r_data :: Word
  , alu_res :: Word
  , reg_dst :: Reg
  , reg_src :: RegSrc
  } deriving (Show, Generic, NFDataX, Default)

-- | Complete processor state
data ProcessorState = ProcessorState
  { ifStage :: IFStage
  , idStage :: IDStage
  , exStage :: EXStage
  , memStage :: MEMStage
  , wbStage :: WBStage
  , registers :: RegisterFile
  } deriving (Show, Generic, NFDataX, Default)

-- | Processor I/O
data ProcessorI = ProcessorI
  { instruction :: Instruction
  , mem_r_data :: Word
  } deriving (Show, Generic, NFDataX, Default)

-- | Processor output
data ProcessorO = ProcessorO
  { pc :: Ptr
  , mem_addr :: Ptr
  , mem_r_en :: Enable
  , mem_w_en :: Enable
  , mem_w_data :: Word
  } deriving (Show, Generic, NFDataX)

iFStageT :: Enable -> PcSrc -> Ptr -> Ptr -> IFStage -> IFStage
iFStageT (Enable pc_w_en) pcSrc (Ptr jump_target) (Ptr branch_target) IFStage {pc=Ptr pc} = IFStage { pc=Ptr pc' }
  where
    pc' = if pc_w_en then pc'' else pc
      where
        pc'' = case pcSrc of
          PcSrc.PC4 -> pc + 4
          JUMP -> jump_target
          BRANCH -> branch_target

-- This would be better written without the monad, but this shows that Haskell monads work as expected.
iDStageT :: Bool -> Enable -> Instruction -> Ptr -> State IDStage ()
iDStageT flush_if_id (Enable w_en_if_id) if_instruction if_pc = do
    s <- get
    if flush_if_id then
      put s {pc = Ptr 0, instruction = Instruction 0}
    else if w_en_if_id then put s {pc = if_pc, instruction = if_instruction}
    else pure ()

-- Extract the reg_dst if the instruction has one.
regDstFromParsedInstruction :: ParsedInstruction -> Maybe Reg
regDstFromParsedInstruction instr = if isSw instr.opcode then Nothing else Just instr.rd

exStageT :: Bool -> Ptr -> Word -> Word -> Word -> InstrDecodeOutput -> ParsedInstruction -> ForwardingUnitOutput -> IDStage -> EXStage
exStageT
  flush_id_ex
  id_jump_target
  immediate
  reg_data_a
  reg_data_b
  InstrDecodeOutput {..}
  parsedInstruction@ParsedInstruction {..}
  ForwardingUnitOutput {..}
  IDStage {..} =
  let branch_target = id_jump_target in
  -- If the register destination doesn't exist default to 0 to false avoid conflicts.
  let reg_dst = fromMaybe (Reg 0) (regDstFromParsedInstruction parsedInstruction) in
  -- Techincally only the `Enable` signals have to actually be reset to `False` to prevent changes to architectural state. The other signals don't matter when all `Enable`s are `False`.
  if flush_id_ex then EXStage {
      pc=Ptr 0
    , opcode=Opcode 0
    , funct3=Funct3 0
    , funct7=Funct7 0
    , immediate=Word 0
    , alu_src_a
    , alu_src_b
    , reg_data_a=Word 0
    , reg_data_b=Word 0
    , mem_r_en=Enable False
    , mem_w_en=Enable False
    , mem_src
    , reg_w_en=Enable False
    , branch=False
    , jump=False
    , branch_target=Ptr 0
    , reg_dst=Reg 0
    , reg_src
  } else EXStage {..}

memStageT :: Word -> Word -> EXStage -> MEMStage
memStageT alu_res mem_w_data EXStage {..} = MEMStage {..}

wbStageT :: Ptr -> ProcessorI -> MEMStage -> WBStage
wbStageT pc_plus_4 ProcessorI {..} MEMStage {..} = WBStage {..}

processorOT :: IFStage -> MEMStage -> ProcessorO
processorOT IFStage {pc} MEMStage {pc=_,..} = ProcessorO {mem_addr=bitCoerce alu_res, ..}

processorT :: ProcessorState -> ProcessorI -> (ProcessorState, ProcessorO)
processorT ProcessorState {..} processorI = (processorState', processorO)
  where
    -- IF
    ifStage' = iFStageT pc_w_en addr_sel jump_target exStage.branch_target ifStage

    -- IF/ID
    idStage' = execState (iDStageT flush_if_id w_en_if_id processorI.instruction ifStage.pc) idStage

    -- ID
    parsedInstruction = parseInstruction idStage.instruction

    instructionDecodeOutput = instrDecode parsedInstruction.opcode

    PipelineCtrlOutput { pc_w_en, addr_sel, flush_if_id, w_en_if_id, flush_id_ex } = pipelineCtrl PipelineCtrlInput {
      ex_branch = exStage.branch,
      ex_equals = equals,
      id_jump = instructionDecodeOutput.jump,
      id_rs1 = parsedInstruction.rs1,
      id_rs2 = parsedInstruction.rs2,
      use_rs1 = instructionDecodeOutput.use_rs1,
      use_rs2 = instructionDecodeOutput.use_rs2,
      ex_mem_r_en = exStage.mem_r_en,
      ex_jump = exStage.jump,
      ex_reg_dst = exStage.reg_dst
    }

    (registers', reg_data_a, reg_data_b) = registersT registers parsedInstruction.rs1 parsedInstruction.rs2 wbStage.reg_w_en wbStage.reg_dst reg_w_data

    immediate = immGen idStage.instruction
    jump_target =
      let Word immediate' = immediate in
      Ptr $ bitCoerce $ bitCoerce idStage.pc + immediate'

    forwardingUnitOutput = forwardingUnit ForwardingUnitInput {
      use_imm = instructionDecodeOutput.use_imm,
      id_rs1 = parsedInstruction.rs1,
      id_rs2 = parsedInstruction.rs2,
      ex_reg_w_en = exStage.reg_w_en,
      mem_reg_w_en = memStage.reg_w_en,
      ex_reg_dst = exStage.reg_dst,
      mem_reg_dst = memStage.reg_dst
    }

    -- ID/EX
    exStage' = exStageT flush_id_ex jump_target immediate reg_data_a reg_data_b instructionDecodeOutput parsedInstruction forwardingUnitOutput idStage

    -- EX
    aluCtrl = aluDecodeT exStage.opcode exStage.funct7 exStage.funct3
    alu_in_a = case exStage.alu_src_a of
      NonImmAluSrc.REG -> exStage.reg_data_a
      NonImmAluSrc.EX -> memStage.alu_res
      NonImmAluSrc.MEM -> reg_w_data
    alu_in_b = case exStage.alu_src_b of
      AluSrc.IMM -> exStage.immediate
      AluSrc.REG -> exStage.reg_data_b
      AluSrc.EX -> memStage.alu_res
      AluSrc.MEM -> reg_w_data
    (alu_res, equals) = aluT aluCtrl alu_in_a alu_in_b

    -- Select the memory write data from the stage determined earlier by the forwarding unit.
    memStage_mem_w_data = case exStage.mem_src of
      NonImmAluSrc.REG -> exStage.reg_data_b
      NonImmAluSrc.EX -> memStage.alu_res -- forward ex stage from previous cycle
      NonImmAluSrc.MEM -> reg_w_data -- forward mem stage from previous cycle

    -- EX/MEM
    memStage' = memStageT alu_res memStage_mem_w_data exStage

    -- MEM
    processorO = processorOT ifStage memStage
    wbStage_pc_plus_4 = Ptr $ bitCoerce memStage.pc + 4

    -- MEM/WB
    wbStage' = wbStageT wbStage_pc_plus_4 processorI memStage

    -- WB
    reg_w_data = case wbStage.reg_src of
        RegSrc.PC4 -> Word $ bitCoerce wbStage.pc_plus_4
        RegSrc.MEM -> wbStage.mem_r_data
        RegSrc.ALU -> wbStage.alu_res

    -- Bundle processor state from pipeline
    processorState' = ProcessorState {
      ifStage=ifStage',
      idStage=idStage',
      exStage=exStage',
      memStage=memStage',
      wbStage=wbStage',
      registers=registers'
    }
