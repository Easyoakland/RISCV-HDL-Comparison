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
The clash code was initialized using `stack new my-clash-project clash-lang/simple`. The old README from this template is available at `README_FROM_TEMPLATE.md`. The `Project.hs` source and test are also from the template, and not related to the processor design.

### Tools
The regular Haskell LSP, [Haskell HLS](https://github.com/haskell/haskell-language-server), can be used for this project like regular for Haskell projects. No extra configuration is needed.

### Testing
`stack test` or `cabal test` will run some simulated tests using CLASH's ability to run as regular Haskell code.

### Build
- `compile_clash.sh` compiles from Haskell/CLASH to systemverilog.
- `verilator_compile_command.md` describes how to use [verilator](https://www.veripool.org/verilator/) (>= 5) to run the design in simulation.
    - The simulation should output the resulting waveform to `test.vcd`. This can be viewed with gtkwave or [surfer](https://gitlab.com/surfer-project/surfer).


[Synthesize](https://hackage-content.haskell.org/package/clash-prelude-1.8.2/docs/Clash-Annotations-TopEntity.html) annotations have been added to the CLASH design so it can simulated and tested using the same testbench as the SystemVerilog version.

## SystemVerilog

### Tools
[svls](https://github.com/dalance/svls-vscode) was used as a linter for this code with the lint configuration located in `.svlint.toml`.

### Build
Build and simulate the design with `verilator_compile_sim.sh`. The resulting waveform should be output to `test.vcd`. This can be viewed with gtkwave or [surfer](https://gitlab.com/surfer-project/surfer).


