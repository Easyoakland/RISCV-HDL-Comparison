`default_nettype none
`include "header.svh"

module ForwardingUnit(
    // If use immediate is true on the instruction.
    input var i_use_imm
    // If register write is enabled per stage.
    , input var i_ex_reg_w_en, i_mem_reg_w_en
    // Instruction register source fields.
    , input var [4:0] i_id_rs1, i_id_rs2
    // Instruction destination register
    , input var [4:0] i_ex_reg_dst, i_mem_reg_dst
    // Which stage/input the alu should read from.
    , output var alu_src::NonImmAluSrc o_alu_src_a
    , output var alu_src::AluSrc o_alu_src_b
    // Which stage/input the data to write to memory should come from.
    , output var alu_src::NonImmAluSrc o_data_mem_src
  );
  // region Forwarding logic for ALU operand A
  always_comb
    // If data is produced one stage ahead in the execute stage.
    // In other words, instruction source register 1 matches the destination of the data in the ex stage and the data is going to be written into the register file.
    if (i_ex_reg_w_en && i_id_rs1 == i_ex_reg_dst && i_ex_reg_dst != 0) o_alu_src_a = alu_src::EX;
    // If data is produced two stages ahead in the memory stage.
    else if (i_mem_reg_w_en && i_id_rs1 == i_mem_reg_dst && i_mem_reg_dst != 0) o_alu_src_a = alu_src::MEM;
    // Data produced in write-back or beyond.
    else o_alu_src_a = alu_src::REG;

  // region Forwarding logic for ALU operand B
  always_comb
    // If immediate instruction don't forward.
    if (i_use_imm) o_alu_src_b = alu_src::IMM;
    // If data is produced one stage ahead in the execute stage.
    else if (i_ex_reg_w_en && i_id_rs2 == i_ex_reg_dst && i_ex_reg_dst != 0) o_alu_src_b = alu_src::EX;
    // If data is produced two stages ahead in the memory stage.
    else if (i_mem_reg_w_en && i_id_rs2 == i_mem_reg_dst && i_mem_reg_dst != 0) o_alu_src_b = alu_src::MEM;
    // Data produced in write-back or beyond.
    else o_alu_src_b = alu_src::REG;

  // region Forwarding logic for data memory
  // Data input to memory comes from reg_b in single-stage design, so the logic here will look the same as reg_b other than not using immediate.
  // It is up to `mem_w_en` in the instruction decode for whether to use this value or not.
  always_comb
    if (i_ex_reg_w_en && i_id_rs2 == i_ex_reg_dst && i_ex_reg_dst != 0) o_data_mem_src = alu_src::EX;
    else if (i_mem_reg_w_en && i_id_rs2 == i_mem_reg_dst && i_mem_reg_dst != 0) o_data_mem_src = alu_src::MEM;
    else o_data_mem_src = alu_src::REG;
endmodule
