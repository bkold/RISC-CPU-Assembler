# RISC-CPU-Assembler
Assembler to accompany the RISC CPU

This program is written with Ada2012 and requires a Ada2012 GNAT compiler.

This system makes use of Getopt, so it is easy to use.
The flags are 
  -a    This tells the compiler to assemble the input
  -d    This tells the compiler to disassemble the input 
  -o:   This designates the output file
  
Example call
  ./compile Sample\ Code.txt -a -o Sample.s

See the RISC-CPU Project for an Op_Codes Document.
