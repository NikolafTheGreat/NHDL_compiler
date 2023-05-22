FIRRTL version 1.1.0
circuit blink__INSTANCE0__ :
  module blink__INSTANCE0__ :
    input clk: Clock
    input res: UInt<1>
    output out: UInt<"b1">
    reg state: UInt<"b1">, clk with: (reset => (res, UInt(0)))
    out <= state
    state <= not(state)