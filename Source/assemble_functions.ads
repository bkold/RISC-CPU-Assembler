with Ada.Strings.Bounded;
with Ada.Strings;
with Ada.Text_IO;
with Ada.Text_IO.Bounded_IO;
with Improved_Trie;
use Ada.Text_IO;

Package Assemble_Functions is
	
	function Build (Source_File, Output_File: in out File_Type) return Boolean;

private
	package SB is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 32);
	package SB_IO is new Ada.Text_IO.Bounded_IO(Bounded=>SB);
	package I_IO is new Ada.Text_IO.Integer_IO(Num=>Integer);
	package Imm_Trie renames Improved_Trie;

	subtype Letter is Character with
		Static_Predicate => Letter in 'A'..'Z' | 'a'..'z';
		
	type Op_Codes is (BAL_32, BEQ_32, BGEZ_32, BGEZAL_32, BGTZ_32, BGTZAL_32, BLTZ_32, BLTZAL_32, BLEZ_32, BLEZAL_32,
		J_32, JAL_32, SJAL_32, JALR_32, JR_32, ADD_32, ADDU_32, AND_32, DIV_32, DIVU_32, MOD_32, MODU_32, MUL_32, MULU_32, NAND_32, 
		NOR_32, OR_32, SUB_32, SUBU_32, SLL_32, SLT_32, SRA_32, SRL_32, SLTU_32, XOR_32, ADDI_32, ANDI_32,
		NORI_32, ORI_32, SLTI_32, SLLI_32, SRAI_32, SRLI_32, SUBI_32, XORI_32, ADDIU_32, SLRIU_32, SUBIU_32, LUI_32, LW_32, SW_32,
		BCPU_32, BCPUJ_32, BCPUJR_32, EXIT_32, SLEEP_32);

	type Registers is (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11,
		r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24,
		r25, r26, r27, r28, r29, r30, r31);

	Current_Line_Number: Positive;
	Instruction_Number: Positive;
	Error_Flag: Boolean;
	Label_Tree: Imm_Trie.Trie.Tree;

	function Assemble (Input: in SB.Bounded_String) return SB.Bounded_String;

	procedure Get_Labels (Source_File: in File_Type);
	procedure Add_Label (Current_Line: in SB.Bounded_String; Instruction_Number: in Integer);

	--parses string
	--RS, RT and RD are Bounded strings that look like binary
	--Op_Code is a Op_Codes type for case statements
	procedure Get_Fields (Input: in SB.Bounded_String; Op_Code: out Op_Codes; Field_1, Field_2, Field_3: out SB.Bounded_String);

	--converts the fields into proper binary
	--calls Get_register and Get_Binary_XX
	procedure Translate_Fields (Op_Code: in Op_Codes; Field_1, Field_2, Field_3: in out SB.Bounded_String; IMM, Base: out SB.Bounded_String);

	--looks up functions
	--gets the special fields for a given opcode
	procedure Get_Specials (Op_Code: in Op_Codes; Special_1, Special_2: out SB.Bounded_String);

	--converts a bounded_string to Op_Codes type
	function Get_Op_Code (Input: in SB.Bounded_String) return Op_Codes;

	--gets binary value of register, in Bounded_string
	function Get_Register (Input: in SB.Bounded_String) return SB.Bounded_String;

	--gets Binary version of integer value
	function Get_Binary_5 (Input: in SB.Bounded_String) return SB.Bounded_String;
	function Get_Binary_16 (Input: in SB.Bounded_String) return SB.Bounded_String;
	function Get_Binary_16_Signed (Input: in SB.Bounded_String) return SB.Bounded_String;
	function Get_Binary_26 (Input: in SB.Bounded_String) return SB.Bounded_String;
	function Get_Binary_Parse (Base_String: in SB.Bounded_String; Num: in Natural; Length: in Positive) return SB.Bounded_String;

	procedure Error_Register (Input: in String);
	procedure Error_Label;
	procedure Error_Opcode (Input: in String);
	procedure Error_Number (Input: in String);

end Assemble_Functions;