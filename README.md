# RISC-CPU-Assembler
Assembler to accompany the RISC CPU

##About
This program is written with Ada2012 and requires a Ada2012 GNAT compiler. The assembler supports labels and comments. See the RISC-CPU Project for an Op_Codes Document. An example code can be seen in 'Sample Code.txt'. At the moment, disassembly is not a built in feature. 

This system makes use of Getopt, so it is easy to use.
The flags are 

    -a    This tells the compiler to assemble the input
    -d    This tells the compiler to disassemble the input 
    -o:   This designates the output file
  
Example call

    ./compile Sample\ Code.txt -a -o Sample.s

