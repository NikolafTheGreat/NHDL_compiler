module ALU__INSTANCE0__(
  input  [1:0]  sel,
  input  [17:0] a,
  input  [17:0] b,
  output [17:0] o
);
  wire [18:0] _GEN_5 = $signed(a) / $signed(b); // @[]
  wire [18:0] _GEN_0 = 2'h3 == sel ? $signed(_GEN_5) : $signed(19'sh0); // @[]
  wire [35:0] _GEN_7 = $signed(a) * $signed(b); // @[]
  wire [35:0] _GEN_8 = {{17{_GEN_0[18]}},_GEN_0}; // @[]
  wire [35:0] _GEN_1 = 2'h2 == sel ? $signed(_GEN_7) : $signed(_GEN_8); // @[]
  wire [18:0] _GEN_9 = $signed(a) - $signed(b); // @[]
  wire [35:0] _GEN_2 = 2'h1 == sel ? $signed({{17{_GEN_9[18]}},_GEN_9}) : $signed(_GEN_1); // @[]
  wire [18:0] _GEN_10 = $signed(a) + $signed(b); // @[]
  wire [35:0] _GEN_3 = 2'h0 == sel ? $signed({{17{_GEN_10[18]}},_GEN_10}) : $signed(_GEN_2); // @[]
  assign o = _GEN_3[17:0];
endmodule
