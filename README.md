# RISC-CPU-Assembler
Assembler to accompany the RISC CPU

## About
This program is written with Ada2012 and requires a Ada2012 GNAT compiler. The assembler supports labels and comments. See the [RISC-CPU Project](https://github.com/bkold/RISC-CPU) for an Op_Codes Document. An example code can be seen in 'example.asm'. At the moment, disassembly is not a built in feature. This assembler is designed to be bullet proof and give intelligent error messages to the user. It will never compile incorrect input or or visa versa. The parser accommodates for tabs as well as spaces and comments. This assembler can easily be retooled to support MIPS32 assembly.

The sample code provided gives many examples for good and bad input.

This system makes use of Getopt, so it is easy to use.
The flags are 

    -a    Tells the compiler to assemble the input - Default
    -d    Tells the compiler to disassemble the input 
    -o:   Designates the output file
  
Example call

    ./risc-cpu-asm example.asm -a -o Sample.s

