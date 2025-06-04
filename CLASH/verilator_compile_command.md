Before adding extra synth commands it was just:

`verilator -j 0 -Wall -Wno-UNOPTFLAT -Wno-UNUSEDSIGNAL -Wno-LITENDIAN -Wno-WIDTH -Wno-TIMESCALEMOD --binary --trace -I systemverilog/PipelinedProc_tb.processor/PipelinedProc_types.sv -I systemverilog/PipelinedProc_tb.processor/PipelinedProc.sv tests/Tests/PipelinedProc_tb.sv --top-module PipelinedProc_tb`

But now that multiple submodules also have to be compiled and included:

`verilator -j 0 -Wall -Wno-UNOPTFLAT -Wno-UNUSEDSIGNAL -Wno-LITENDIAN -Wno-WIDTH -Wno-TIMESCALEMOD --binary --trace -I systemverilog/InstrDecode.parseInstruction/parseInstruction_types.sv systemverilog/InstrDecode.parseInstruction/parseInstruction.sv -I systemverilog/RegisterFile.registersT/RegisterFile_types.sv -I systemverilog/RegisterFile.registersT/RegisterFile.sv -I systemverilog/Alu.aluT/ALU_types.sv -I systemverilog/Alu.aluT/ALU.sv -I systemverilog/ForwardingUnit.forwardingUnit/ForwardingUnit_types.sv -I systemverilog/ForwardingUnit.forwardingUnit/ForwardingUnit.sv -I systemverilog/PipelinedProc_tb.processor/PipelinedProc_types.sv -I systemverilog/PipelinedProc_tb.processor/PipelinedProc.sv tests/Tests/PipelinedProc_tb.sv --top-module PipelinedProc_tb`
