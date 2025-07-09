`default_nettype none

module InstrDecode(
    // instruction op field
    input var [6:0] i_opcode
    // assert when immediate field is input to ALU
    , output var o_use_imm
    // assert when instruction is a jump
    , output var o_jump
    // assert when instruction is a branch
    , output var o_branch
    // assert when reading mem
    , output var o_mem_r_en
    // assert when writing mem
    , output var o_mem_w_en
    // select source to write to register file
    , output var instr_decode::RegSrc o_reg_src
    // assert when writing to register file
    , output var o_reg_w_en
    // assert when rs{1,2} in use (source is register file)
    , output var o_use_rs1, o_use_rs2
  );

  always_comb unique case (i_opcode)
    // R-type arithmetic instruction
    'b0110011: begin
      o_use_imm = 0;
      o_jump = 0;
      o_branch = 0;
      o_mem_r_en = 0;
      o_mem_w_en = 0;
      o_reg_src = instr_decode::ALU; // ALU output
      o_reg_w_en = 'b1; // reg write
      o_use_rs1 = 'b1; // use rs1
      o_use_rs2 = 'b1; // use rs2
    end
    // I-type arithmetic instructions
    'b0010011: begin
      o_use_imm = 'b1; // use immediate
      o_jump = 0;
      o_branch = 0;
      o_mem_r_en = 0;
      o_mem_w_en = 0;
      o_reg_src = instr_decode::ALU; // ALU output
      o_reg_w_en = 'b1; // reg write
      o_use_rs1 = 'b1; // use rs1
      o_use_rs2 = 'b0;
    end
    // lw
    'b0000011: begin
      o_use_imm = 'b1; // use immediate
      o_jump = 0;
      o_branch = 0;
      o_mem_r_en = 'b1; // mem read
      o_mem_w_en = 0;
      o_reg_src = instr_decode::MEM; // data memory
      o_reg_w_en = 'b1; // reg write
      o_use_rs1 = 'b1; // use rs1
      o_use_rs2 = 'b0; // use rs2
    end
    // sw
    'b0100011: begin
      o_use_imm = 'b1; // use immediate
      o_jump = 0;
      o_branch = 0;
      o_mem_r_en = 0;
      o_mem_w_en = 'b1; // mem write
      o_reg_src = instr_decode::RegSrc'('x);
      o_reg_w_en = 0;
      o_use_rs1 = 'b1; // use rs1
      o_use_rs2 = 'b1; // use rs2
    end
    // beq
    'b1100011: begin
      o_use_imm = 0;
      o_jump = 0;
      o_branch = 'b1; // branch
      o_mem_r_en = 0;
      o_mem_w_en = 0;
      o_reg_src = instr_decode::RegSrc'('x);
      o_reg_w_en = 0;
      o_use_rs1 = 'b1; // use rs1
      o_use_rs2 = 'b1; // use rs2
    end
    /* jalr
    'b1100111: begin
    end */
    // jal
    'b1101111: begin
      // Immediate is source of jump, but doesn't go to the ALU
      // This would be where PCsrc would be set to the immediate but this module doesn't handle that
      o_use_imm = 0;
      o_jump = 'b1; // jump
      o_branch = 0;
      o_mem_r_en = 0;
      o_mem_w_en = 0;
      o_reg_src = instr_decode::PC4; // and link
      o_reg_w_en = 'b1; // reg write "link"
      o_use_rs1 = 'b0;
      o_use_rs2 = 'b0;
    end
    // lui
    'b0110111: begin
      o_use_imm = 'b1; // use immediate
      o_jump = 0;
      o_branch = 0;
      o_mem_r_en = 0;
      o_mem_w_en = 0;
      o_reg_src = instr_decode::ALU; // ALU output
      o_reg_w_en = 'b1; // write reg
      o_use_rs1 = 0;
      o_use_rs2 = 0;
    end
    // Unsupported/invalid instructions shouldn't change state
    default: begin
      o_use_imm = 1'bx; // don't care
      o_jump = 0; // don't jump
      o_branch = 0; // don't branch
      o_mem_r_en = 0; // don't touch mem
      o_mem_w_en = 0; // don't touch mem
      o_reg_src = instr_decode::RegSrc'('x); // don't care
      o_reg_w_en = 0; // don't write to reg
      o_use_rs1 = 0; // not using rs1
      o_use_rs2 = 0; // not using rs2
    end
  endcase
endmodule
