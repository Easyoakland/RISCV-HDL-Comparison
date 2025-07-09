`default_nettype none

module RegisterFile(
    input var logic [4:0] i_r_reg_a
    , input var logic [4:0] i_r_reg_b
    , input var logic [4:0] i_w_reg
    , input var logic i_clk
    , input var logic i_w_reg_enable
    , input var logic [31:0] i_w_data
    , output var logic [31:0] o_r_data_a
    , output var logic [31:0] o_r_data_b
);

    typedef logic [31:0] Register;
    typedef Register[31:1] RegisterArray;
    RegisterArray registers;

    // Writes occur on the falling edge of the clock so instructions can write before dependent instruction reads. This is an implicit forwarding path for register writes then reads.
    always_ff @(negedge i_clk)
        if (i_w_reg_enable == 1 && i_w_reg != 0) registers[i_w_reg] <= i_w_data;

    always_comb
        if (i_r_reg_a == 0) o_r_data_a = 0;
        else o_r_data_a = registers[i_r_reg_a];

    always_comb
        if (i_r_reg_b == 0) o_r_data_b = 0;
        else o_r_data_b = registers[i_r_reg_b];
endmodule
