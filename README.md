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

----------------------
-labels are surrounded by barackets, no spaces for name
-comments are lines starting with one dash
-you may write code and finish with a comment
-only one instruction per line
----------------------
-example code
[Label_1]
Add_32 r1, r2, r3 -add_32 rd, rs, rt
Add_32 r4, r5, r6 
J_32 Label_2

[Label_2]
Add_32 r1, r2, r3
J_32 Label_1
