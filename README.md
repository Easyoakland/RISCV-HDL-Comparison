This repository implements a (partial) RISCV processor in several hardware description languages as a comparison point.

# Design

The design is based on the 5 stage pipelined processor with forwarding from "Computer Organization and Design RISC-V Edition: THe Hardware Software interface" by David A. Patterson & John L. Hennessy. Branch prediction follows the "assume not taken" strategy.

## Figure

TODO

## Supported Instructions

- Arithemtic — ADD, ADDI, SUB
- Logical — AND, ANDI, OR, ORI, XOR, XORI
- Shift — SLL, SLLI, SRL, SRLI, SRA, SRAI
- Comparison — SLT, SLTI, SLTU, SLTIU
- Loads and stores — LW and SW
- Control Flow — BEQ and JAL
- Load Upper Immediate — LUI

# Languages
## CLASH
The clash code was initialized using `stack new my-clash-project clash-lang/simple`. The old README from this template is available at `README_FROM_TEMPLATE.md`.

## Testing
`stack test` or `cabal test` will run some simulated tests using CLASH's ability to run as regular Haskell code.

### Build
- `compile_clash.sh` compiles from Haskell/CLASH to systemverilog.
- `verilator_compile_command.md` describes how to use [verilator](https://www.veripool.org/verilator/) (>= 5) to run the design in simulation.

[Synthesize](https://hackage-content.haskell.org/package/clash-prelude-1.8.2/docs/Clash-Annotations-TopEntity.html) annotations have been added to the CLASH design so it can simulated and tested using the same testbench as the SystemVerilog version.

## SystemVerilog

TODO
