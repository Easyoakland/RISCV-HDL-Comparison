`default_nettype none
`include "header.svh"

/*
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

Signals attempt to adhere to the following format:
{i,o,}_stage_signal

The {i,o,} prefix indicates if the signal is an input or output or neither if local to module.
The `stage` identifies where in the pipeline the signal originates. Multiple stage prefixes imply the signal originates from the pipeline register between the indicated stages.
For example, o_id_ex_mem_r_en is an output signal, originating from the register between the instruction decode and execute stages, indicating if the memory read should be enabled.
For example, i_id_mem_r_en is an input signal, originating from within the instruction decode stage, indicating if the memory read should be enabled.
*/

module PipelinedProc (
    input var i_clk, i_rst_l
    , output var [31:0] o_pc
    , input var [31:0] i_instruction
    , output var [31:0] o_data_mem_addr
    , output var o_data_mem_r_en, o_data_mem_w_en
    , input var [31:0] i_data_mem_r_data
    , output var [31:0] o_data_mem_w_data
  );

  // region IF
  logic [31:0] if_pc;

  // PC value mux.
  // Synchronous reset low for pc, only write if pc_w_en, choose pc based on jump or branch or regular increment by 4.
  always_ff @(posedge i_clk)
    if (i_rst_l == 0) if_pc <= 0;
    else if (pc_w_en) begin
      unique case (id_addr_sel)
        pc_src::PC4: if_pc <= if_pc + 4;
        pc_src::JUMP: if_pc <= if_id_pc + id_immediate; // jump resolves in the id stage.
        pc_src::BRANCH: if_pc <= id_ex_pc + id_ex_immediate; // branch resolves in the ex stage.
        default: if_pc <= 'x; // unreachable
      endcase
    end
    else if_pc <= if_pc;
  assign o_pc = if_pc;

  // region IF_ID
  // Pipeline control signals.
  logic [31:0] if_id_instruction;
  logic flush_if_id, w_en_if_id, pc_w_en;
  logic [31:0] if_id_pc;
  always_ff @(posedge i_clk)
    if (flush_if_id) begin
        if_id_instruction <= 0;
        if_id_pc <= 0;
    end
    else if (w_en_if_id == 'b1) begin
      if_id_instruction <= i_instruction;
      if_id_pc <= if_pc;
    end;

  // region ID
  // Instruction decode signals.
  logic [4:0] id_rs1, id_rs2, id_rd;
  logic id_use_rs1, id_use_rs2;
  alu_src::AluSrc id_alu_src_a, id_alu_src_b;
  logic [31:0] id_immediate;
  logic [6:0] id_opcode;
  logic [2:0] id_funct3;
  logic [6:0] id_funct7;
  logic id_use_imm;
  logic id_jump;
  logic id_branch;
  logic id_data_mem_r_en;
  logic id_data_mem_w_en;
  instr_decode::RegSrc id_reg_src;
  logic id_reg_w_en;
  logic [31:0] id_reg_r_data_a;
  logic [31:0] id_reg_r_data_b;
  alu_src::NonImmAluSrc id_data_mem_src;
  logic flush_id_ex;
  pc_src::PCSrc id_addr_sel;

  assign id_opcode = if_id_instruction[6:0];
  assign id_rd = if_id_instruction[11:7];
  assign id_funct3 = if_id_instruction[14:12];
  assign id_rs1 = if_id_instruction[19:15];
  assign id_rs2 = if_id_instruction[24:20];
  assign id_funct7 = if_id_instruction[31:25];

  InstrDecode u_instr_decode(
    .i_opcode(id_opcode)
    , .o_use_imm(id_use_imm)
    , .o_jump(id_jump)
    , .o_branch(id_branch)
    , .o_mem_r_en(id_data_mem_r_en)
    , .o_mem_w_en(id_data_mem_w_en)
    , .o_reg_src(id_reg_src)
    , .o_reg_w_en(id_reg_w_en)
    , .o_use_rs1(id_use_rs1)
    , .o_use_rs2(id_use_rs2)
  );

  PipelineCtrl u_pipeline_ctrl(
    .i_ex_branch(id_ex_branch)
    , .i_ex_equals(ex_equals)
    , .i_id_jump(id_jump)
    , .i_id_rs1(id_rs1), .i_id_rs2(id_rs2)
    , .i_use_rs1(id_use_rs1), .i_use_rs2(id_use_rs2)
    , .i_ex_mem_r_en(id_ex_data_mem_r_en)
    , .i_ex_jump(id_ex_jump)
    , .i_ex_reg_dst(id_ex_w_reg)
    , .o_pc_w_en(pc_w_en)
    , .o_addr_sel(id_addr_sel)
    , .o_flush_if_id(flush_if_id)
    , .o_w_en_if_id(w_en_if_id)
    , .o_flush_id_ex(flush_id_ex)
  );

  RegisterFile u_registers(
    .i_r_reg_a(id_rs1)
    , .i_r_reg_b(id_rs2)
    , .i_w_reg(mem_wb_w_reg)
    , .i_clk(i_clk)
    , .i_w_reg_enable(mem_wb_reg_w_en)
    , .i_w_data(wb_reg_w_data)
    , .o_r_data_a(id_reg_r_data_a)
    , .o_r_data_b(id_reg_r_data_b)
  );

  ImmGen u_imm_gen(.i_instruction(if_id_instruction), .o_immediate(id_immediate));

  ForwardingUnit u_forwarding_unit(
    .i_use_imm(id_use_imm)
    , .i_ex_reg_w_en(id_ex_reg_w_en), .i_mem_reg_w_en(ex_mem_reg_w_en)
    , .i_id_rs1(id_rs1), .i_id_rs2(id_rs2)
    , .i_ex_reg_dst(id_ex_w_reg), .i_mem_reg_dst(ex_mem_w_reg)
    , .o_alu_src_a(id_alu_src_a)
    , .o_alu_src_b(id_alu_src_b)
    , .o_data_mem_src(id_data_mem_src)
  );

  // All these signals are synchronously reset by `flush_id_ex` so that a stall/slip/bubble/nop instruction passes through.
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_immediate <= 0; else id_ex_immediate <= id_immediate;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_reg_r_data_a <= 0; else id_ex_reg_r_data_a <= id_reg_r_data_a;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_reg_r_data_b <= 0; else id_ex_reg_r_data_b <= id_reg_r_data_b;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_data_mem_r_en <= 0; else id_ex_data_mem_r_en <= id_data_mem_r_en;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_data_mem_w_en <= 0; else id_ex_data_mem_w_en <= id_data_mem_w_en;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_opcode <= 0; else id_ex_opcode <= id_opcode;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_funct3 <= 0; else id_ex_funct3 <= id_funct3;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_funct7 <= 0; else id_ex_funct7 <= id_funct7;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_data_mem_src <= alu_src::AluSrc'(0); else id_ex_data_mem_src <= id_data_mem_src;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_w_reg <= 0; else id_ex_w_reg <= id_rd;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_pc <= 0; else id_ex_pc <= if_id_pc;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_branch <= 0; else id_ex_branch <= id_branch;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_alu_src_a <= alu_src::AluSrc'(0); else id_ex_alu_src_a <= id_alu_src_a;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_alu_src_b <= alu_src::AluSrc'(0); else id_ex_alu_src_b <= id_alu_src_b;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_reg_w_en <= 0; else id_ex_reg_w_en <= id_reg_w_en;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_jump <= 0; else id_ex_jump <= id_jump;
  always_ff @(posedge i_clk) if (flush_id_ex) id_ex_reg_src <= instr_decode::RegSrc'(0); else id_ex_reg_src <= id_reg_src;

  // region ID_EX
  logic [2:0] id_ex_funct3;
  logic [6:0] id_ex_funct7;
  alu_src::NonImmAluSrc id_ex_alu_src_a;
  alu_src::AluSrc id_ex_alu_src_b;
  logic [31:0] id_ex_immediate;
  logic [31:0] id_ex_reg_r_data_a;
  logic [31:0] id_ex_reg_r_data_b;
  logic id_ex_data_mem_r_en;
  logic id_ex_data_mem_w_en;
  logic [6:0] id_ex_opcode;
  alu_src::NonImmAluSrc id_ex_data_mem_src;
  logic [4:0] id_ex_w_reg;
  logic [31:0] id_ex_pc;
  logic id_ex_branch;
  logic id_ex_reg_w_en;
  logic id_ex_jump;
  instr_decode::RegSrc id_ex_reg_src;

  // region EX
  // Execute signals
  logic [31:0] ex_alu_in_a, ex_alu_in_b;
  logic [31:0] ex_alu_res;
  logic ex_equals;
  alu_decode::AluCtrl ex_alu_ctrl;

  always_comb unique case (id_ex_alu_src_a)
    // Regular register usage by instruction. No forwarding needed.
    alu_src::REG: ex_alu_in_a = id_ex_reg_r_data_a;
    // Bypass path from execute stage for using alu output on next cycle. On this cycle will be in the register after ex.
    alu_src::EX: ex_alu_in_a = ex_mem_alu_res;
    // Bypass path from memory stage on previous cycle. On this cycle will be getting written back in WB stage.
    alu_src::MEM: ex_alu_in_a = wb_reg_w_data;
    default: ex_alu_in_a = 'x; // unreachable
  endcase
  always_comb unique case (id_ex_alu_src_b)
    // Source from immediate generation unit. No forwarding needed.
    alu_src::IMM: ex_alu_in_b = id_ex_immediate;
    // Regular register usage by instruction. No forwarding needed.
    alu_src::REG: ex_alu_in_b = id_ex_reg_r_data_b;
    // Bypass path from execute stage for using alu output on next cycle. On this cycle will be in the register after ex.
    alu_src::EX: ex_alu_in_b = ex_mem_alu_res;
    // Bypass path from memory stage on previous cycle. On this cycle will be getting written back in WB.
    alu_src::MEM: ex_alu_in_b = wb_reg_w_data;
    default: ex_alu_in_b = 'x; // unreachable
  endcase

  ALUDecode u_alu_decode(
    .i_opcode(id_ex_opcode)
    , .i_funct3(id_ex_funct3)
    , .i_funct7(id_ex_funct7)
    , .o_alu_ctrl(ex_alu_ctrl)
  );

  ALU u_alu(
    .i_alu_ctrl(ex_alu_ctrl)
    , .i_alu_a(ex_alu_in_a)
    , .i_alu_b(ex_alu_in_b)
    , .o_alu_res(ex_alu_res)
    , .o_equals(ex_equals)
  );

  // Select the memory write data from the stage determined earlier by the forwarding unit.
  always_ff @(posedge i_clk) unique case (id_ex_data_mem_src)
    alu_src::REG: ex_mem_data_mem_w_data <= id_ex_reg_r_data_b; // regular read from register file
    alu_src::EX: ex_mem_data_mem_w_data <= ex_mem_alu_res; // forward ex stage value from previous cycle
    alu_src::MEM: ex_mem_data_mem_w_data <= wb_reg_w_data; // forward mem stage value from previous cycle
    default: ex_mem_data_mem_w_data <= 'x; // unreachable
  endcase

  always_ff @(posedge i_clk) ex_mem_w_reg <= id_ex_w_reg;
  always_ff @(posedge i_clk) ex_mem_pc <= id_ex_pc;
  always_ff @(posedge i_clk) ex_mem_reg_src <= id_ex_reg_src;
  always_ff @(posedge i_clk) ex_mem_mem_r_en <= id_ex_data_mem_r_en;
  always_ff @(posedge i_clk) ex_mem_mem_w_en <= id_ex_data_mem_w_en;
  always_ff @(posedge i_clk) ex_mem_reg_w_en <= id_ex_reg_w_en;
  always_ff @(posedge i_clk) ex_mem_alu_res <= ex_alu_res;

  // region EX/MEM
  // EX/MEM pipeline registers
  logic ex_mem_mem_r_en, ex_mem_mem_w_en; // ex_mem stage memory read enable and write enable
  instr_decode::RegSrc ex_mem_reg_src;
  logic ex_mem_reg_w_en;
  logic [31:0] ex_mem_alu_res;
  logic [31:0] ex_mem_data_mem_w_data; // ex_mem stage, for data memory, data to write.
  logic [4:0] ex_mem_w_reg;
  logic [31:0] ex_mem_pc;

  // region MEM
  // Read from memory
  always_ff @(posedge i_clk) mem_wb_data_mem_r_data <= i_data_mem_r_data;
  assign o_data_mem_r_en = ex_mem_mem_r_en;

  // Write to memory.
  assign o_data_mem_w_data = ex_mem_data_mem_w_data;
  assign o_data_mem_w_en = ex_mem_mem_w_en;

  // If this instruction is reading or writing memory then the address is the output of the alu.
  assign o_data_mem_addr = ex_mem_alu_res;

  always_ff @(posedge i_clk) mem_wb_w_reg <= ex_mem_w_reg;
  always_ff @(posedge i_clk) mem_wb_pc <= ex_mem_pc;
  always_ff @(posedge i_clk) mem_wb_reg_src <= ex_mem_reg_src;
  always_ff @(posedge i_clk) mem_wb_reg_w_en <= ex_mem_reg_w_en;
  always_ff @(posedge i_clk) mem_wb_alu_res <= ex_mem_alu_res;

  // region MEM/WB
  // MEM/WB pipeline registers
  instr_decode::RegSrc mem_wb_reg_src;
  logic mem_wb_reg_w_en;
  logic [31:0] mem_wb_data_mem_r_data;
  logic [31:0] mem_wb_alu_res;
  logic [31:0] mem_wb_pc;
  logic [4:0] mem_wb_w_reg; // sent all the way back to register file in the id stage.

  // region WB
  // Writeback stage signals
  logic [31:0] wb_reg_w_data;
  // Write-back stage final value mux is either output of memory, alu, or pc+4 for `jal`.
  always_comb unique case (mem_wb_reg_src)
    instr_decode::MEM: wb_reg_w_data = mem_wb_data_mem_r_data;
    instr_decode::ALU: wb_reg_w_data = mem_wb_alu_res;
    instr_decode::PC4: wb_reg_w_data = mem_wb_pc + 4;
    default: wb_reg_w_data = 'x; // unreachable
  endcase
endmodule
