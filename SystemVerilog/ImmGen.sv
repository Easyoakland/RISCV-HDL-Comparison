`default_nettype none

module ImmGen(
    input var [31:0] i_instruction // instruction to decode immediate from
    , output var signed [31:0] o_immediate // immediate is sign extended
  );
  var logic [6:0] opcode;
  assign opcode = i_instruction[6:0];
  always_comb unique case (opcode)
    // I-type arithmetic instructions
    'b0010011: o_immediate = 32'(signed'(i_instruction[31:20]));
    // lw
    'b0000011: o_immediate = 32'(signed'(i_instruction[31:20]));
    // sw
    'b0100011: o_immediate = 32'(signed'({i_instruction[31:25], i_instruction[11:7]}));
    // lui
    'b0110111: o_immediate = 32'(signed'({i_instruction[31:12], 12'b0}));
    // beq
    'b1100011: o_immediate = 32'(signed'({i_instruction[31], i_instruction[7], i_instruction[30:25], i_instruction[11:8], 1'b0}));
    // jal
    'b1101111: o_immediate = 32'(signed'({i_instruction[31], i_instruction[19:12], i_instruction[20], i_instruction[30:21], 1'b0}));
    default: o_immediate = 'x; // the immediate value isn't used so don't care
  endcase
endmodule
