`default_nettype none
`ifndef HEADER_SVH  // guard
`define HEADER_SVH

package alu_decode;
    typedef enum logic [3:0] {
        AND = 'b0000
        , OR = 'b0001
        , ADD = 'b0010
        , SLL = 'b0011
        , SRL = 'b0100
        , SUB = 'b0110
        , SLT = 'b0111
        , XOR = 'b1010
        , SLTU = 'b1011
        , SRA = 'b1101
        , LUI = 'b1110
    } AluCtrl;

endpackage

package instr_decode;
    // 00 => PC+4 (for "and link" part of jal)
    // 01 => data memory (load)
    // 10 => ALU
    typedef enum logic [1:0] { PC4='b00, MEM='b01, ALU='b10 } RegSrc;
endpackage

package alu_src;
    typedef enum logic [1:0] {
        // Use Immediate
        IMM='b00
        // Use Execute bypass
        , EX='b10
        // Use Memory bypass
        , MEM='b01
        // Pull from register file
        , REG='b11
    } AluSrc;
    // NonImmAluSrc conceptually represents the enum below. Can't do that because enum below conflicts because System Verilog namespacing is weird.
    typedef AluSrc NonImmAluSrc;

    // typedef enum logic [1:0] {
    //     // Use Execute bypass
    //     EX='b10
    //     // Use Memory bypass
    //     , MEM='b01
    //     // Pull from register file
    //     , REG='b11
    // } NonImmAluSrc;
endpackage

package pc_src;
  typedef enum logic [1:0] {
    // Next PC is PC+4
    PC4 = 'b00
    // Next PC is from jump
    , JUMP = 'b01
    // Next PC is from branch
    , BRANCH = 'b10
  } PCSrc;
endpackage

`endif  // guard
