verilator -j 0 -Wall -Wno-DECLFILENAME -Wno-MODDUP --binary --trace -I *.sv -I *.svh PipelinedProc_tb.sv --top-module PipelinedProc_tb
./obj_dir/VPipelinedProc_tb
