FIRRTL version 1.1.0
circuit top__INSTANCE0__ :
  module shift__INSTANCE0__ :
    input clk: Clock
    input next: UInt<"b1">
    output output: UInt<"b00010010">
    reg state: UInt<"b1">["b00010010"], clk
    state[0] <= next
    node out = state["b0"]
    state[1] <= state["b00"]
    node out0 = cat(state["b1"], out)
    state[2] <= state["b001"]
    node out1 = cat(state["b10"], out0)
    state[3] <= state["b010"]
    node out2 = cat(state["b11"], out1)
    state[4] <= state["b0011"]
    node out3 = cat(state["b100"], out2)
    state[5] <= state["b0100"]
    node out4 = cat(state["b101"], out3)
    state[6] <= state["b0101"]
    node out5 = cat(state["b110"], out4)
    state[7] <= state["b0110"]
    node out6 = cat(state["b111"], out5)
    state[8] <= state["b00111"]
    node out7 = cat(state["b1000"], out6)
    state[9] <= state["b01000"]
    node out8 = cat(state["b1001"], out7)
    state[10] <= state["b01001"]
    node out9 = cat(state["b1010"], out8)
    state[11] <= state["b01010"]
    node out10 = cat(state["b1011"], out9)
    state[12] <= state["b01011"]
    node out11 = cat(state["b1100"], out10)
    state[13] <= state["b01100"]
    node out12 = cat(state["b1101"], out11)
    state[14] <= state["b01101"]
    node out13 = cat(state["b1110"], out12)
    state[15] <= state["b01110"]
    node out14 = cat(state["b1111"], out13)
    state[16] <= state["b001111"]
    node out15 = cat(state["b10000"], out14)
    state[17] <= state["b010000"]
    node out16 = cat(state["b10001"], out15)
    output <= out16
  module top__INSTANCE0__ :
    input clk: Clock
    input next: UInt<"b1">
    output output: UInt<"b00010010">
    inst register of shift__INSTANCE0__
    register.next <= next
    register.clk <= clk
    output <= register.output