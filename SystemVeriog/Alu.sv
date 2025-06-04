`default_nettype none
`include "header.svh"

module ALU(
    input var alu_decode::AluCtrl i_alu_ctrl
    , input var logic [31:0] i_alu_a, i_alu_b
    , output var logic [31:0] o_alu_res
    , output var logic o_equals
  );
  assign o_equals = i_alu_a == i_alu_b;

  always_comb unique case (i_alu_ctrl)
    alu_decode::AND: o_alu_res = i_alu_a & i_alu_b;
    alu_decode::OR: o_alu_res = i_alu_a | i_alu_b;
    alu_decode::ADD: o_alu_res = i_alu_a + i_alu_b;
    alu_decode::SLL: o_alu_res = i_alu_a << i_alu_b;
    alu_decode::SRL: o_alu_res = i_alu_a >> i_alu_b;
    alu_decode::SUB: o_alu_res = i_alu_a - i_alu_b;
    // SLT
    // Sets the destination register to 1 if the first source register is less than
    // the second source register when both are treated as signed numbers, otherwise it sets
    // the destination register to 0.
    alu_decode::SLT: o_alu_res = {31'b0, signed'(i_alu_a) < signed'(i_alu_b)};
    alu_decode::XOR: o_alu_res = i_alu_a ^ i_alu_b;
    // SLTU
    // Same as SLT but unsigned
    alu_decode::SLTU: o_alu_res = {31'b0, i_alu_a < i_alu_b};
    alu_decode::SRA: o_alu_res = i_alu_a >>> i_alu_b;
    // LUI
    // Load the upper 20 bits. The intermediate generation unit will output the already shifted and corrected value. The ALU only needs to pass on the already shifted value since the ImmGen doesn't directly connect to `reg_w_data`. Also note the intermediate output goes to input `i_alu_b` not `i_alu_a`.
    alu_decode::LUI: o_alu_res = i_alu_b;
    default: o_alu_res = 'x;
  endcase
endmodule

module TestBench;
  logic [3:0] alu_ctrl;
  logic [31:0] alu_a, alu_b, alu_res;
  logic Equals;
  ALU u_alu (
    .i_alu_ctrl (alu_ctrl)
    , .i_alu_a (alu_a)
    , .i_alu_b (alu_b)
    , .o_alu_res (alu_res)
    , .o_equals (Equals)
  );
  initial begin
    // Setup trace for opening in gtkwave
    $dumpfile("test.vcd"); // Customize trace location.
    $dumpvars(0, top); // Set values to be dumped. First value is level (0 for all, 1+ for number of submodules).

    $display("Starting Test");
    // Test LUI
    #10 alu_ctrl = 'b1110; alu_a = 'b11111000010001001011111111111111; alu_b = 1; #10
    // LUI: Bottom 12 unset but but top 20 is the same.
    if (alu_res == 'b11111000010001001011000000000000); else $error("LUI: %b", alu_res);

    // Test SLT
    #10 alu_ctrl = 'b0111; alu_a = -'sd1; alu_b = 'sd1; #10
    if (alu_res == 'd1); else $error("SLT: %b", alu_res);

    #10 alu_ctrl = 'b0111; alu_a = 'sd1; alu_b = 'sd1; #10
    if (alu_res == 'd0); else $error("SLT: %b", alu_res);

    #10 alu_ctrl = 'b0111; alu_a = -'sd2; alu_b = 'sd1; #10
    if (alu_res == 'd1); else $error("SLT: %b", alu_res);

    #10 alu_ctrl = 'b0111; alu_a = 'sd2; alu_b = 'sd1; #10
    if (alu_res == 'd0); else $error("SLT: %b", alu_res);

    // Test SLTU
    #10 alu_ctrl = 'b1011; alu_a = -'sd2; alu_b = 'sd1; #10
    if (alu_res == 'd0); else $error("SLTU: %b", alu_res);

    #10 alu_ctrl = 'b1011; alu_a = 'sd2; alu_b = 'sd1; #10
    if (alu_res == 'd0); else $error("SLTU: %b", alu_res);

    #10 alu_ctrl = 'b1011; alu_a = 'sd0; alu_b = 'sd1; #10
    if (alu_res == 'd1); else $error("SLTU: %b", alu_res);

    // Equals
    #10 alu_ctrl = 'b1011; alu_a = 'sd0; alu_b = 'sd1; #10
    if (Equals == 'd0); else $error("Equals: %b", Equals);

    #10 alu_ctrl = 'b1011; alu_a = 'sd1; alu_b = 'sd1; #10
    if (Equals == 'd1); else $error("Equals: %b", Equals);

    $finish();
  end
endmodule
