stack run -- clash --systemverilog tests/Tests/PipelinedProc_tb.hs -main-is processor
stack run -- clash --systemverilog src/InstrDecode.hs -main-is parseInstruction
stack run -- clash --systemverilog src/RegisterFile.hs -main-is registersT
stack run -- clash --systemverilog src/Alu.hs -main-is aluT
stack run -- clash --systemverilog src/ForwardingUnit.hs -main-is forwardingUnit
