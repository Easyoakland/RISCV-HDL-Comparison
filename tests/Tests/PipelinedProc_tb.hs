{-# OPTIONS_GHC -Wno-orphans #-}
module PipelinedProc_tb where

import Clash.Prelude
import Types.Newtype (Ptr)
import PipelinedProc

-- Use a custom active low reset domain to match the original system verilog code.
-- The default "System" domain is active high.
createDomain vSystem{vName="ActiveLowRst", vResetPolarity = ActiveLow}
{-# ANN processor (Synthesize {
  t_name = "PipelinedProc",
  t_inputs = [PortName "i_clk", PortName "i_rst_l", PortProduct "" [PortName "i_instruction", PortName "i_data_mem_r_data"]],
  t_output = PortProduct "" [PortName "o_pc", PortName "o_data_mem_addr", PortName "o_data_mem_r_en", PortName "o_data_mem_w_en", PortName "o_data_mem_w_data"]
}) #-}
{-# NOINLINE processor #-}
processor :: Clock ActiveLowRst -> Reset ActiveLowRst -> Signal ActiveLowRst ProcessorI -> Signal ActiveLowRst ProcessorO
processor clk rst = exposeClockResetEnable (mealy processorT def) clk rst enableGen

------------------------------------------------------------------
--Testbench assembly
--       add x5, x0, x0
--       li x7, 0xfeedbeef --pseudo instruction that will turn into a lui and addi
--       sw x7, 0(x5)
--       addi x6, x5, 4
--       lw x8, 0(x5)
--       sw x8, 0(x6)
--       add x6, x6, x6
--       j jumpTarget --pseudo instruction that will turn into a jal
--       andi x7, x7, 0
--jumpTarget:
--       sw x7, 0(x6)
--       andi x7, x8, 0
--       sw x7, 4(x6)
--       add x7, x0, x0
--       ori x5, x0, 0x10
--       xor x8, x8, x8
--loop:
--       addi x8, x8, 1
--       beq x7, x5, exitLoop
--       addi x7, x7, 1
--       j loop
--exitLoop:
--       sw x7, 8(x6)
--       sw x8, 12(x6)
--       lw x8, 12(x6)
--       slli x8, x8, 2
--       sw x8, 16(x6)
--       srli x9, x8, 1
--       sw x9, 20(x6)
--       ori x7, x0, 0x1
--       or x5, x7, x0
--       xori x5, x5, -1
--       sltu x8, x7, x5
--       sw x8, 24(x6)
--       sltu x8, x5, x7
--       sw x8, 28(x6)
--       sltiu x8, x5, 0xffff
--       sw x8, 32(x6)
--       sltiu x8, x7, 0xffff
--       sw x8, 36(x6)
--       slt x8, x7, x5
--       sw x8, 40(x6)

-- create vector array for memories
type MemArray = Vec 64 (BitVector 32)

-- signals for Instruction memory
instrMem :: MemArray
instrMem =
    0x000002b3 :> -- add t0,zero,zero
    0xfeedc3b7 :> -- lui t2,0xfeedc
    0xeef38393 :> -- addi t2,t2,-273 # feedbeef <exitLoop+0xfeedbe9f>
    0x0072a023 :> -- sw t2,0(t0)
    0x00428313 :> -- addi t1,t0,4
    0x0002a403 :> -- lw s0,0(t0)
    0x00832023 :> -- sw s0,0(t1)
    0x00630333 :> -- add t1,t1,t1
    0x0080006f :> -- j 28 <jumpTarget>
    0x0003f393 :> -- andi t2,t2,0
    --jumpTarget:
    0x00732023 :> -- sw t2,0(t1)
    0x00047393 :> -- andi t2,s0,0
    0x00732223 :> -- sw t2,4(t1)
    0x000003b3 :> -- add t2,zero,zero
    0x01006293 :> -- ori t0,zero,16
    0x00844433 :> -- xor s0,s0,s0
    --loop:
    0x00140413 :> -- addi s0,s0,1
    0x00538663 :> -- beq t2,t0,50 <exitLoop>
    0x00138393 :> -- addi t2,t2,1
    0xff5ff06f :> -- j 40 <loop>
    --exitLoop:
    0x00732423 :> -- sw t2,8(t1)
    0x00832623 :> -- sw s0,12(t1)
    0x00c32403 :> -- lw s0,12(t1)
    0x00241413 :> -- slli s0,s0,0x2
    0x00832823 :> -- sw s0,16(t1)
    0x00145493 :> -- srli s1,s0,0x1
    0x00932a23 :> -- sw s1,20(t1)
    0x00106393 :> -- ori t2,zero,1
    0x0003e2b3 :> -- or t0,t2,zero
    0xfff2c293 :> -- not t0,t0
    0x0053b433 :> -- sltu s0,t2,t0
    0x00832c23 :> -- sw s0,24(t1)
    0x0072b433 :> -- sltu s0,t0,t2
    0x00832e23 :> -- sw s0,28(t1)
    0xfff2b413 :> -- sltiu s0,t0,-1
    0x02832023 :> -- sw s0,32(t1)
    0xfff3b413 :> -- sltiu s0,t2,-1
    0x02832223 :> -- sw s0,36(t1)
    0x0053a433 :> -- slt s0,t2,t0
    0x02832423 :> -- sw s0,40(t1)
    repeat 0x00000000 -- nop for default

pcAlignAssert :: HiddenClockResetEnable dom => Signal dom Ptr -> Signal dom Bool
pcAlignAssert pc = (== 0) . slice d1 d0 <$> pc

expectedMemory :: [(Index 64, BitVector 32)]
expectedMemory = [
    (0, 0xfeedbeef)  -- Initial magic value
    , (1, 0xfeedbeef)  -- Copied magic value
    , (2, 0xfeedbeef)  -- Same value at doubled address
    , (3, 0)           -- Cleared value
    , (4, 16)          -- Loop limit
    , (5, 17)          -- Final loop counter
    , (6, 68)          -- 17 << 2
    , (7, 34)          -- 68 >> 1
    , (8, 1)           -- Comparison result: 1 < 0xfffffffe
    , (9, 0)           -- Comparison result: 0xfffffffe >= 1
    , (10, 1)          -- Comparison result: 0xfffffffe >= 0xffff
    , (11, 1)          -- Comparison result: 1 < 0xffff
    , (12, 0)          -- Signed comparison: 1 >= -2
  ]
