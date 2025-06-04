`default_nettype none
`include "header.svh"

module ALUDecode(
    input var [6:0] i_opcode
    , input var [6:0] i_funct7
    , input var [2:0] i_funct3
    , output var alu_decode::AluCtrl o_alu_ctrl
  );

  always_comb
    // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#lui
    if (i_opcode == 'b0110111) o_alu_ctrl = alu_decode::LUI;
    // Loads and stores perform adds due to offset calculation.
    // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sw
    else if (i_opcode == 'b0100011) o_alu_ctrl = alu_decode::ADD;
    // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#lw
    else if (i_opcode == 'b0000011) o_alu_ctrl = alu_decode::ADD;
    else if (i_opcode == 'b0010011) begin // i-type
        unique case(i_funct3)
          // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#andi
          'b111: o_alu_ctrl = alu_decode::AND;
          // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#ori
          'b110: o_alu_ctrl = alu_decode::OR;
          // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#addi
          'b000: o_alu_ctrl = alu_decode::ADD;
          // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#xori
          'b100: o_alu_ctrl = alu_decode::XOR;
          // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#slli
          'b001: o_alu_ctrl = alu_decode::SLL;
          'b101:
            // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#srai
            if (i_funct7[5]) o_alu_ctrl = alu_decode::SRA;
            // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#srli
            else o_alu_ctrl = alu_decode::SRL;
          // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#slti
          'b010: o_alu_ctrl = alu_decode::SLT;
          // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sltiu
          'b011: o_alu_ctrl = alu_decode::SLTU;
          default: o_alu_ctrl = alu_decode::AluCtrl'('x); // invalid value
        endcase
    end
    else if (i_opcode == 'b0110011) begin // r-type
      unique case({i_funct7, i_funct3})
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#and
        'b0000000111: o_alu_ctrl = alu_decode::AND;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#or
        'b0000000110: o_alu_ctrl = alu_decode::OR;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#add
        'b0000000000: o_alu_ctrl = alu_decode::ADD;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sll
        'b0000000001: o_alu_ctrl = alu_decode::SLL;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#srl
        'b0000000101: o_alu_ctrl = alu_decode::SRL;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sub
        'b0100000000: o_alu_ctrl = alu_decode::SUB;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#slt
        'b0000000010: o_alu_ctrl = alu_decode::SLT;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#xor
        'b0000000100: o_alu_ctrl = alu_decode::XOR;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sltu
        'b0000000011: o_alu_ctrl = alu_decode::SLTU;
        // https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sra
        'b0100000101: o_alu_ctrl = alu_decode::SRA;
        default: o_alu_ctrl = alu_decode::AluCtrl'('x); // invalid value
      endcase
    end
endmodule
