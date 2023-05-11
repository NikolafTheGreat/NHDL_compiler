FIRRTL version 1.1.0
circuit MyModule
  module MyModule :
    input a: UInt<UInt(32)>, 
    input b: SInt<UInt(3)>, 
    output out: SInt<UInt(3)>[UInt(2)], 
    n <= add(add(SInt("h-abc"), UInt(2)), mul(UInt(1), SInt(-3)))