`default_nettype none
`include "header.svh"

module PipelineCtrl(
    input var i_ex_branch // to detect a branch in EX
    , input var i_ex_equals // to resolve the branch in EX
    , input var i_id_jump // to detect a jump in ID
    , input var [4:0] i_id_rs1, i_id_rs2
    , input var i_use_rs1, i_use_rs2 // to detect if rs1 or rs2 is used
    , input var i_ex_mem_r_en // to detect a load in EX
    , input var i_ex_jump // to detect a jal in EX
    , input var [4:0] i_ex_reg_dst
    , output var o_pc_w_en // to control writes to PC
    , output var pc_src::PCSrc o_addr_sel // select where to get next PC
    , output var o_flush_if_id // to flush instruction in fetch
    , output var o_w_en_if_id // to control writes to IF/ID reg
    , output var o_flush_id_ex // to flush instruction in decode
  );

  // Execute stage determined the branch is taken if instruction is branch and registers are equal.
  var ex_branch_taken;
  assign ex_branch_taken = i_ex_branch && i_ex_equals;

  // region Flush stages

  // Potential memory stage data hazard at rs1 if rs1 in use (source is register file) and value is in execute stage.
  var rs1_mem_data_hazard;
  assign rs1_mem_data_hazard = i_ex_reg_dst == i_id_rs1 && i_use_rs1;
  // Potential memory stage data hazard at rs2 if source is not immediate (source is register file) and value is in execute stage.
  // Should probably make a use_rs2 signal instead to avoid unnecessary stalls but not miss sw stalls.
  var rs2_mem_data_hazard;
  assign rs2_mem_data_hazard = i_ex_reg_dst == i_id_rs2 && i_use_rs2;
  // Potential memory stage data hazard next cycle on either rs1 or rs2. The potential hazard is only realized as an actual hazard if the instruction's value is only generated in the memory stage.
  var mem_data_hazard;
  assign mem_data_hazard = rs1_mem_data_hazard || rs2_mem_data_hazard;

  always_comb
    // Flush instructions in the fetch and decode stages on branch taken in execute stage.
    if (ex_branch_taken) begin
      o_flush_if_id = 'b1;
      o_w_en_if_id = 'b1; // store the instruction at the target of the branch
      o_flush_id_ex = 'b1;
      o_pc_w_en = 'b1;
    end
    // Flush instructions in fetch stage if jump in decode stage.
    else if (i_id_jump) begin
      o_flush_if_id = 'b1;
      o_w_en_if_id = 'b1; // store the instruction at the target of the jump
      o_flush_id_ex = 'b0;
      o_pc_w_en = 'b1;
    end
    // If a memory stage data hazard occurs in the memory stage (dependency on lw instruction or jal), stall the fetch and decode stages *and* don't update the program counter (no jump full slip).
    // This is called a "slip".
    // TODO why is jal pc+4 calculated in the memory stage instead of earlier and forwarded/passed regularly to avoid slip?
    else if (mem_data_hazard && (i_ex_mem_r_en || i_ex_jump)) begin
      o_flush_if_id = 'b0; // Don't flush the value on slip, ...
      o_w_en_if_id = 'b0; // ... instead hold it by disabling writes to if/id. The decode stage will process the same instruction next cycle...
      o_flush_id_ex = 'b1; // ... and flush the value in the id/ex register to avoid side-effects of the slipped instruction. The instruction in id_ex is now the same as the instruction in if_id but the copy in the id_ex register has an un-forwardable data dependency and so it shouldn't be run.
      o_pc_w_en = 'b0;
    end
    // Don't flush or slip otherwise.
    else begin
      o_flush_if_id = 'b0;
      o_w_en_if_id = 'b1;
      o_flush_id_ex = 'b0;
      o_pc_w_en = 'b1;
    end;

  // endregion
  // region PC select

  always_comb
    if (i_id_jump) o_addr_sel = pc_src::JUMP;
    else if (ex_branch_taken) o_addr_sel = pc_src::BRANCH;
    else o_addr_sel = pc_src::PC4;
endmodule
