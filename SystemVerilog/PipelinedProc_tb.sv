//////////////////////////////////////////////////////////////////
//Testbench assembly
//       add x5, x0, x0
//       li x7, 0xfeedbeef //pseudo instruction that will turn into a lui and addi
//       sw x7, 0(x5)
//       addi x6, x5, 4
//       lw x8, 0(x5)
//       sw x8, 0(x6)
//       add x6, x6, x6
//       j jumpTarget //pseudo instruction that will turn into a jal
//       andi x7, x7, 0
//jumpTarget:
//       sw x7, 0(x6)
//       andi x7, x8, 0
//       sw x7, 4(x6)
//       add x7, x0, x0
//       ori x5, x0, 0x10
//       xor x8, x8, x8
//loop:
//       addi x8, x8, 1
//       beq x7, x5, exitLoop
//       addi x7, x7, 1
//       j loop
//exitLoop:
//       sw x7, 8(x6)
//       sw x8, 12(x6)
//       lw x8, 12(x6)
//       slli x8, x8, 2
//       sw x8, 16(x6)
//       srli x9, x8, 1
//       sw x9, 20(x6)
//       ori x7, x0, 0x1
//       or x5, x7, x0
//       xori x5, x5, -1
//       sltu x8, x7, x5
//       sw x8, 24(x6)
//       sltu x8, x5, x7
//       sw x8, 28(x6)
//       sltiu x8, x5, 0xffff
//       sw x8, 32(x6)
//       sltiu x8, x7, 0xffff
//       sw x8, 36(x6)
//       slt x8, x7, x5
//       sw x8, 40(x6)

module PipelinedProc_tb();
    var logic clk, rst_l;

    // create vector array for memories
    typedef logic [63:0][31:0] mem_array;

    //signals for Instruction memory
    // {<<8{32'h000002b3}} changes endian uses reversing streaming operator on blocks of size 8 (1 byte)
    const mem_array InstrMem = '{
        0 : 'h000002b3 //add t0,zero,zero
        , 1 : 'hfeedc3b7 //lui t2,0xfeedc
        , 2 : 'heef38393 //addi t2,t2,-273 # feedbeef <exitLoop+0xfeedbe9f>
        , 3 : 'h0072a023 //sw t2,0(t0)
        , 4 : 'h00428313 //addi t1,t0,4
        , 5 : 'h0002a403 //lw s0,0(t0)
        , 6 : 'h00832023 //sw s0,0(t1)
        , 7 : 'h00630333 //add t1,t1,t1
        , 8 : 'h0080006f //j 28 <jumpTarget>
        , 9 : 'h0003f393 //andi t2,t2,0
         //jumpTarget:
        , 10 : 'h00732023 //sw t2,0(t1)
        , 11 : 'h00047393 //andi t2,s0,0
        , 12 : 'h00732223 //sw t2,4(t1)
        , 13 : 'h000003b3 //add t2,zero,zero
        , 14 : 'h01006293 //ori t0,zero,16
        , 15 : 'h00844433 //xor s0,s0,s0
        //loop:
        , 16 : 'h00140413 //addi s0,s0,1
        , 17 : 'h00538663 //beq t2,t0,50 <exitLoop>
        , 18 : 'h00138393 //addi t2,t2,1
        , 19 : 'hff5ff06f //j 40 <loop>
        //exitLoop:
        , 20 : 'h00732423 //sw t2,8(t1)
        , 21 : 'h00832623 //sw s0,12(t1)
        , 22 : 'h00c32403 //lw s0,12(t1)
        , 23 : 'h00241413 //slli s0,s0,0x2
        , 24 : 'h00832823 //sw s0,16(t1)
        , 25 : 'h00145493 //srli s1,s0,0x1
        , 26 : 'h00932a23 //sw s1,20(t1)
        , 27 : 'h00106393 //ori t2,zero,1
        , 28 : 'h0003e2b3 //or t0,t2,zero
        , 29 : 'hfff2c293 //not t0,t0
        , 30 : 'h0053b433 //sltu s0,t2,t0
        , 31 : 'h00832c23 //sw s0,24(t1)
        , 32 : 'h0072b433 //sltu s0,t0,t2
        , 33 : 'h00832e23 //sw s0,28(t1)
        , 34 : 'hfff2b413 //sltiu s0,t0,-1
        , 35 : 'h02832023 //sw s0,32(t1)
        , 36 : 'hfff3b413 //sltiu s0,t2,-1
        , 37 : 'h02832223 //sw s0,36(t1)
        , 38 : 'h0053a433 //slt s0,t2,t0
        , 39 : 'h02832423 //sw s0,40(t1)
        , default: 'h00000000 //nop
    };

    /* verilator lint_off UNUSEDSIGNAL */
    var logic [31:0] PC, Instruction;
    /* verilator lint_off UNUSEDSIGNAL */

    // signals for Data memory
    /* verilator lint_off UNUSEDSIGNAL */
    var logic [31:0] DataMemAddr;
    /* verilator lint_off UNUSEDSIGNAL */
    var logic [31:0] DataMemRdData;
    var logic [31:0] DataMemWrData;
    var logic DataMemRdEn, DataMemWrEn;
    var mem_array DataMem;

    // instantiate UUT
    // use nominal port map as opposed to positional
    PipelinedProc u_uut(
        .i_clk(clk)
        , .i_rst_l (rst_l)
        , .o_pc (PC)
        , .i_instruction (Instruction)
        , .o_data_mem_addr (DataMemAddr)
        , .o_data_mem_r_en (DataMemRdEn)
        , .o_data_mem_w_en (DataMemWrEn)
        , .i_data_mem_r_data (DataMemRdData)
        , .o_data_mem_w_data (DataMemWrData)
        );

    // concurrent process to generate a 100MHz clock signal
    always_ff begin
        clk <= 0; # 5ns;
        clk <= 1; # 5ns;
    end;

    // Stimulus process
    initial begin: stim_proc
        $dumpfile("test.vcd"); // Customize trace location.
        $dumpvars(0); // Set values to be dumped. First value is level (0 for all, 1+ for number of submodules).

        // hold reset state for 2 clocks.
        rst_l = 0; #20ns;
        rst_l = 1; // release processor out of reset

        // Check the jump instruction
        // This is commented out because it isn't easy to calculate the cycle-accurate timing because the pipelined processor takes variable cycles per instruction compared to the single-cycle design.
        /* #80ns;
        if (Instruction == 'h0080006f); else $error("not on j");
        #10ns;
        if (Instruction == 'h00732023); else $error("j failed"); */

        #1300ns // Wait for a while

        // Assert the memory state matches how it should be at the end
        if (DataMem[0] != 'hfeedbeef) $error("mem[0] fail");
        if (DataMem[1] != 'hfeedbeef) $error("mem[1] fail");
        if (DataMem[2] != 'hfeedbeef) $error("mem[2] fail");
        if (DataMem[3] != 0) $error("mem[3] fail");
        if (DataMem[4] != 16) $error("mem[4] fail");
        if (DataMem[5] != 17) $error("mem[5] fail");
        if (DataMem[6] != 68) $error("mem[6] fail");
        if (DataMem[7] != 34) $error("mem[7] fail");
        if (DataMem[8] != 1) $error("mem[8] fail");
        if (DataMem[9] != 0) $error("mem[9] fail");
        if (DataMem[10] != 1) $error("mem[10] fail");
        if (DataMem[11] != 1) $error("mem[11] fail");
        if (DataMem[12] != 0) $error("mem[12] fail");

        // Assert the program took the expected number of cycles.
        // This is commented out because it isn't easy to calculate the cycle-accurate timing because the pipelined processor takes variable cycles per instruction compared to the single-cycle design.
        // This assert fails if the current cycle is after the program finished.
        /* if (Instruction == 0) $error("last instruction fail");
        #10ns // wait a cycle
        // And now the program should be past the useful instructions.
        if (Instruction != 0) $error("last+1 instruction not nop"); */
        $finish(); // Terminate the simulation.
    end;

    // Instruction ROM read logic.
    assign Instruction = InstrMem[PC[7:2]]; //word aligned

    // Ensure 2 lsb of PC are never nonzero (PC always align(4)).
    int clk_num = 0;
    always_ff @(clk)
        clk_num <= clk_num + 1;
    always_comb if (PC[1:0] == 0);
      else $error("unaligned PC at clk: %d", clk_num);

    // Data memory synchronous write logic
    // on the rising edge of clock
    // only do something if clock changes
    always_ff @ (posedge clk)
        // only write if enabled
        if (DataMemWrEn == 1) DataMem[DataMemAddr[7:2]] <= DataMemWrData;

    // Data memory asynchronous read logic
    always_comb
        if(DataMemRdEn == 1) DataMemRdData = DataMem[unsigned'(DataMemAddr[7:2])]; // word aligned
        else DataMemRdData = '{default: 'z}; // high impedance out
endmodule
