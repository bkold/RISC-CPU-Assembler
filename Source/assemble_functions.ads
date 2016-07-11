with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Bounded;
with Ada.Text_IO;
with Ada.Text_IO.Bounded_IO;
with Improved_Trie;
use Ada.Text_IO;

Package Assemble_Functions is
	
	--receives the input file and a created output file.
	--returns a completed output file and whether the operation had errors or not
	function Build (Source_File, Output_File: in out File_Type) return Boolean;

private
	package SB is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 32);
	package SB_Long is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 255);
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

	Current_Line_Number: Positive:= 1;
	Instruction_Number: Positive:= 1;
	Error_Flag: Boolean;
	Label_Tree: Imm_Trie.Trie.Tree;
	Tab: constant Character:= Character'Val(9);
	White_Space_Sequ: constant Ada.Strings.Maps.Character_Sequence := ' ' & Tab;
	White_Space: constant Ada.Strings.Maps.Character_Set:= Ada.Strings.Maps.To_Set(White_Space_Sequ);

	--receives a line of assembly and returns a finished binary string.
	function Assemble (Input: in SB.Bounded_String) return SB.Bounded_String;

	--returns a clean bounded string. No tabs, not leading and trailing space, and no comment
	function Pull_Clean_Line(Source_File: in File_Type) return SB.Bounded_String;

	--iterates the file, finding labels: [EXAMPLE]
	procedure Get_Labels (Source_File: in out File_Type);

	--adds the found label to a trie
	procedure Add_Label (Current_Line: in SB.Bounded_String; Instruction_Number: in Integer);

	--parses string
	--Op_Code is a Op_Codes type for case statements
	--Field_1-3 are the operands for an instruction
	procedure Get_Fields (Input: in SB.Bounded_String; Op_Code: out Op_Codes; Field_1, Field_2, Field_3: out SB.Bounded_String);

	--converts the fields into proper binary
	--calls Get_register and Get_Binary_XX
	procedure Translate_Fields (Op_Code: in Op_Codes; Field_1, Field_2, Field_3: in out SB.Bounded_String; IMM, Base: out SB.Bounded_String);

	--looks up functions
	--gets the special fields for a given opcode
	procedure Get_Specials (Op_Code: in Op_Codes; Special_1, Special_2: out SB.Bounded_String);

	--converts a bounded_string to Op_Codes type
	function Get_Op_Code (Input: in SB.Bounded_String) return Op_Codes;

	--gets binary value of register
	function Get_Register (Input: in SB.Bounded_String) return SB.Bounded_String;

	--gets Binary version of integer value
	function Get_Binary_5 (Input: in SB.Bounded_String) return SB.Bounded_String;
	function Get_Binary_16 (Input: in SB.Bounded_String) return SB.Bounded_String;
	function Get_Binary_16_Signed (Input: in SB.Bounded_String) return SB.Bounded_String;
	function Get_Binary_26 (Input: in SB.Bounded_String) return SB.Bounded_String;

	function Get_Binary_16_Signed_Label (Input: in SB.Bounded_String) return SB.Bounded_String;
	function Get_Binary_26_Label (Input: in SB.Bounded_String) return SB.Bounded_String;
	--generic parsing function to be used by the above Get_Binary functions
	function Get_Binary_Parse (Base_String: in SB.Bounded_String; Num: in Natural; Length: in Positive) return SB.Bounded_String;

	--called when unexpected errors occure from incorrect inputs. Sets the Error_Flag to true.
	procedure Error_Register (Input: in String);
	procedure Error_Label;
	procedure Error_Opcode (Input: in String);
	procedure Error_Number (Input: in String);

end Assemble_Functions;