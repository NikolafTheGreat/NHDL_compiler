FIRRTL version 1.1.0
circuit ALU__INSTANCE0__ :
  module ALU__INSTANCE0__ :
    input sel: UInt<2>
    input a: SInt<"b00010010">
    input b: SInt<"b00010010">
    output o: SInt<"b00010010">
    when eq(UInt(0), sel) :
      o <- add(a, b)
    else :
      when eq(UInt(1), sel) :
        o <- sub(a, b)
      else :
        when eq(UInt(2), sel) :
          o <- mul(a, b)
        else :
          when eq(UInt(3), sel) :
            o <- div(a, b)
          else :
            o <- SInt<"b00010010">(0)