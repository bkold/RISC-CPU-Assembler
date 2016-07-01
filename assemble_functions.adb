Package body Assemble_Functions is

	function  Build (Source_File, Output_File: in File_Type) return Boolean is
			Current_Line: SB.Bounded_String;

		begin
			Error_Flag:= False;
			Current_Line_Number:=1;
			while not End_Of_File(Source_File) loop
				Current_Line:=SB_IO.Get_Line(Source_File);
				SB_IO.Put_Line(Output_File, Assemble(Current_Line));
				Current_Line_Number:=Current_Line_Number+1;
			end loop;
			return Error_Flag;
	end Build;


	function Assemble (Input: in SB.Bounded_String) return SB.Bounded_String is
			Op_Code: Op_Codes;
			Field_1: SB.Bounded_String;
			Field_2: SB.Bounded_String;
			Field_3: SB.Bounded_String;
			IMM: SB.Bounded_String;
			Base: SB.Bounded_String;
			Special_1: SB.Bounded_String;
			Special_2: SB.Bounded_String;

			Output: SB.Bounded_String;

		begin
			Get_Fields(Input, Op_Code, Field_1, Field_2, Field_3);
			Translate_Fields(Op_Code, Field_1, Field_2, Field_3, IMM, Base);
			Get_Specials(Op_Code, Special_1, Special_2);

			case Op_Code is  
				when BAL_32 =>
					SB.Append(Output, "0000010000010001");
					SB.Append(Output, IMM);

				when BEQ_32 =>
					SB.Append(Output, "000011");
					SB.Append(Output, Field_1);
					SB.Append(Output, Field_2);
					SB.Append(Output, IMM);

				when BGEZ_32..BLEZAL_32 => 				
					SB.Append(Output, "000001");
					SB.Append(Output, Field_1);
					SB.Append(Output, Special_1);
					SB.Append(Output, IMM);

				when J_32..SJAL_32 =>
					SB.Append(Output, Special_1);
					SB.Append(Output, IMM);

				when JALR_32..JR_32 =>
					SB.Append(Output, "000001");
					SB.Append(Output, Field_1);
					SB.Append(Output, Special_1);
					SB.Append(Output, "0000000000000000");

				when ADD_32..XOR_32 =>
					SB.Append(Output, "000000");
					SB.Append(Output, Field_2);
					SB.Append(Output, Field_3);
					SB.Append(Output, Field_1);
					SB.Append(Output, Special_1);
					SB.Append(Output, Special_2);

				when ADDI_32..SUBIU_32 =>
					SB.Append(Output, Special_1);
					SB.Append(Output, Field_2);
					SB.Append(Output, Field_1);
					SB.Append(Output, IMM);

				when LUI_32..SW_32 =>
					SB.Append(Output, Special_1);
					SB.Append(Output, Base);
					SB.Append(Output, Field_1);
					SB.Append(Output, IMM);

				when BCPU_32 =>
					SB.Append(Output, "001100");
					SB.Append(Output, Field_1);
					SB.Append(Output, Field_2);
					SB.Append(Output, "0000000000000000");

				when BCPUJ_32 =>
					SB.Append(Output, "001101");
					SB.Append(Output, IMM);

				when BCPUJR_32 => 
					SB.Append(Output, "001111");
					SB.Append(Output, Field_1);
					SB.Append(Output, "000000000000000000000");

				when EXIT_32 => 
					SB.Append(Output, "00100100000000000000000000000000");

				when SLEEP_32 =>
					SB.Append(Output, "00100000000");
					SB.Append(Output, Field_2);
					SB.Append(Output, Field_1);

			end case;

			return Output;

		exception
			when Constraint_Error => return Output;

	end Assemble;


	procedure Get_Fields(Input: in SB.Bounded_String; Op_Code: out Op_Codes; Field_1, Field_2, Field_3: out SB.Bounded_String) is
			Index_Start: Natural;
			Index_End: Natural;

		begin
			Index_Start:=SB.Index_Non_Blank(Input, 1);
			Index_End:= SB.Index(Input, " ", Index_Start);
			if Index_End = 0 then
				Op_Code:= Get_Op_Code(SB.Bounded_Slice(Input, Index_Start, SB.Length(Input)));
			else 
				Op_Code:= Get_Op_Code(SB.Bounded_Slice(Input, Index_Start, Index_End-1));
			end if;

			Index_Start:= SB.Index_Non_Blank(Input, Index_End+1);
			Index_End:= SB.Index(Input, ",", Index_Start);
			if Index_End = 0 then
				Field_1:= (SB.Bounded_Slice(Input, Index_Start, SB.Length(Input)));
			else 
				Field_1:= (SB.Bounded_Slice(Input, Index_Start, Index_End-1));
			end if;

			Index_Start:= SB.Index_Non_Blank(Input, Index_End+1);
			Index_End:= SB.Index(Input, ",", Index_Start);
			if Index_End = 0 then
				Field_2:= (SB.Bounded_Slice(Input, Index_Start, SB.Length(Input)));
			else 
				Field_2:= (SB.Bounded_Slice(Input, Index_Start, Index_End-1));
			end if;

			Index_Start:= SB.Index_Non_Blank(Input, Index_End+1);
			Index_End:= SB.Index(Input, ",", Index_Start);
			if Index_End = 0 then
				Field_3:= (SB.Bounded_Slice(Input, Index_Start, SB.Length(Input)));
			else 
				Field_3:= (SB.Bounded_Slice(Input, Index_Start, Index_End-1));
			end if;

		exception 
			when others => return;

	end Get_Fields;


	procedure Translate_Fields (Op_Code: in Op_Codes; Field_1, Field_2, Field_3: in out SB.Bounded_String; IMM, Base: out SB.Bounded_String) is
			Index_Start: Natural:= 1;
			Index_End: Natural:= 1;

		begin
			case Op_Code is  
				when BAL_32 =>
					IMM:= Get_Binary_16_Signed(Field_1);

				when BEQ_32 =>
					Field_1:= Get_Register(Field_1);
					Field_2:= Get_Register(Field_2);
					IMM:= Get_Binary_16_Signed(Field_3);

				when BGEZ_32..BLEZAL_32 => 
					Field_1:= Get_Register(Field_1);
					IMM:= Get_Binary_16_Signed(Field_2);

				when J_32..SJAL_32 =>
					IMM:= Get_Binary_26(Field_1);

				when JALR_32..JR_32 =>
					Field_1:= Get_Register(Field_1);

				when ADD_32..XOR_32 =>
					Field_1:= Get_Register(Field_1);
					Field_2:= Get_Register(Field_2);
					Field_3:= Get_Register(Field_3);

				when ADDI_32..XORI_32 =>
					Field_1:= Get_Register(Field_1);
					Field_2:= Get_Register(Field_2);
					IMM:= Get_Binary_16(Field_3);

				when ADDIU_32..SUBIU_32 =>
					Field_1:= Get_Register(Field_1);
					Field_2:= Get_Register(Field_2);
					IMM:= Get_Binary_16_Signed(Field_3);

				when LUI_32 => 
					Field_1:= Get_Register(Field_1);
					IMM:= Get_Binary_16_Signed(Field_2);
					Base:= SB.To_Bounded_String("00000");

				when LW_32..SW_32 =>
					Field_1:= Get_Register(Field_1);
					Index_Start:= SB.Index(Field_2, "(", 1);
					Index_End:= SB.Index(Field_2, ")", Index_Start);
					Base:= Get_Binary_5(SB.Bounded_Slice(Field_2, Index_Start+1, Index_End-1));
					IMM:= Get_Binary_16_Signed(SB.Bounded_Slice(Field_2, 1, Index_Start-1));

				when BCPU_32 =>
					Field_1:= Get_Binary_5(Field_1);
					Field_2:= Get_Binary_5(Field_2);
						
				when BCPUJ_32 =>
					IMM:= Get_Binary_26(Field_1);

				when BCPUJR_32 => 
					Field_1:= Get_Register(Field_1);

				when SLEEP_32 =>
					Field_1:= Get_Binary_5(Field_1);
					Field_2:= Get_Binary_16(Field_2);

				when others =>
					return;

			end case;

	end Translate_Fields;


	procedure Get_Specials (Op_Code: in Op_Codes; Special_1, Special_2: out SB.Bounded_String) is

		begin
			case Op_Code is  

				when BGEZ_32 => Special_1:= SB.To_Bounded_String("00010");

				when BGEZAL_32 => Special_1:= SB.To_Bounded_String("10010");

				when BGTZ_32 => Special_1:= SB.To_Bounded_String("00011");

				when BGTZAL_32 => Special_1:= SB.To_Bounded_String("10011");

				when BLTZ_32 => Special_1:= SB.To_Bounded_String("00100");

				when BLTZAL_32 => Special_1:= SB.To_Bounded_String("10100");

				when BLEZ_32 => Special_1:= SB.To_Bounded_String("00101");

				when BLEZAL_32 => Special_1:= SB.To_Bounded_String("10101");
				
				when J_32 => Special_1:= SB.To_Bounded_String("000101");

				when JAL_32 => Special_1:= SB.To_Bounded_String("000111");

				when SJAL_32 => Special_1:= SB.To_Bounded_String("001111");

				when JALR_32 => Special_1:= SB.To_Bounded_String("11000");

				when JR_32 => Special_1:= SB.To_Bounded_String("01000");

				when ADD_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("001000");

				when ADDU_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("001001");

				when AND_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("100000");

				when DIV_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("000100");

				when DIVU_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("000101");

				when MOD_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("000110");

				when MODU_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("000111");

				when MUL_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("100110");

				when MULU_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("100111");

				when NAND_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("100001");

				when NOR_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("010000");

				when OR_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("110000");
				
				when SUB_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("001100");	

				when SUBU_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("001101");	

				when SLL_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("100100");	

				when SLT_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("101100");	

				when SRA_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("000011");	

				when SRL_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("000010");	

				when SLTU_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("101101");	

				when XOR_32 => Special_1:= SB.To_Bounded_String("00000");
					Special_2:=SB.To_Bounded_String("111000");	

				when ADDI_32 => Special_1:= SB.To_Bounded_String("101000");

				when ADDIU_32 => Special_1:= SB.To_Bounded_String("10100I");

				when ANDI_32 => Special_1:= SB.To_Bounded_String("110000");

				when NORI_32 => Special_1:= SB.To_Bounded_String("110001");

				when ORI_32 => Special_1:= SB.To_Bounded_String("110010");

				when SLTI_32 => Special_1:= SB.To_Bounded_String("101100");

				when SLLI_32 => Special_1:= SB.To_Bounded_String("110100");

				when SRAI_32 => Special_1:= SB.To_Bounded_String("110101");

				when SRLI_32 => Special_1:= SB.To_Bounded_String("110110");

				when SUBI_32 => Special_1:= SB.To_Bounded_String("101110");

				when SUBIU_32 => Special_1:= SB.To_Bounded_String("101111");

				when XORI_32 => Special_1:= SB.To_Bounded_String("101111");

				when LUI_32 => Special_1:= SB.To_Bounded_String("011001");

				when LW_32 => Special_1:= SB.To_Bounded_String("010001");
				
				when SW_32 => Special_1:= SB.To_Bounded_String("010011");

				when others => return;

			end case;

		exception
			when Constraint_Error => Error_Opcode(Op_Codes'Image(Op_Code));				

	end Get_Specials;

	
	function Get_Op_Code (Input: in SB.Bounded_String) return Op_Codes is
	
		begin	
			return Op_Codes'Value(SB.To_String(Input));

		exception
			when Constraint_Error => 
				Error_Opcode(SB.To_String(Input));
				return BAL_32;

	end Get_Op_Code;


	function Get_Register (Input: in SB.Bounded_String) return SB.Bounded_String is
			Reg: Registers;

		begin
			Reg:= Registers'Value(SB.To_String(Input));
			case Reg is
				when r0 => return SB.To_Bounded_String("00000");
				when r1 => return SB.To_Bounded_String("00001");
				when r2 => return SB.To_Bounded_String("00010");
				when r3 => return SB.To_Bounded_String("00011");
				when r4 => return SB.To_Bounded_String("00100");
				when r5 => return SB.To_Bounded_String("00101");
				when r6 => return SB.To_Bounded_String("00110");
				when r7 => return SB.To_Bounded_String("00111");
				when r8 => return SB.To_Bounded_String("01000");
				when r9 => return SB.To_Bounded_String("01001");
				when r10 => return SB.To_Bounded_String("01010");
				when r11 => return SB.To_Bounded_String("01011");
				when r12 => return SB.To_Bounded_String("01100");
				when r13 => return SB.To_Bounded_String("01101");
				when r14 => return SB.To_Bounded_String("01110");
				when r15 => return SB.To_Bounded_String("01111");
				when r16 => return SB.To_Bounded_String("10000");
				when r17 => return SB.To_Bounded_String("10001");
				when r18 => return SB.To_Bounded_String("10010");
				when r19 => return SB.To_Bounded_String("10011");
				when r20 => return SB.To_Bounded_String("10100");
				when r21 => return SB.To_Bounded_String("10101");
				when r22 => return SB.To_Bounded_String("10110");
				when r23 => return SB.To_Bounded_String("10111");
				when r24 => return SB.To_Bounded_String("11000");
				when r25 => return SB.To_Bounded_String("11001");
				when r26 => return SB.To_Bounded_String("11010");
				when r27 => return SB.To_Bounded_String("11011");
				when r28 => return SB.To_Bounded_String("11100");
				when r29 => return SB.To_Bounded_String("11101");
				when r30 => return SB.To_Bounded_String("11110");
				when r31 => return SB.To_Bounded_String("11111");
			end case;

		exception
			when Constraint_Error => 
				Error_Register(SB.To_String(Input));
				return SB.To_Bounded_String("00000");

	end Get_Register;


	function Get_Binary_5 (Input: in SB.Bounded_String) return SB.Bounded_String is
			Out_Num: SB.Bounded_String;
			Done: SB.Bounded_String:= SB.To_Bounded_String("00000");
			Temp: String:= "                    ";
			type Integer_5 is range 0..31;
			Temp_Integer: Integer_5;

		begin
			Temp_Integer:= Integer_5'Value(SB.To_String(Input));
			I_IO.Put(To=>Temp, Item=>Integer(Temp_Integer), Base=>2);
			Out_Num:= SB.To_Bounded_String(Temp);
			Out_Num:= SB.Bounded_Slice(Out_Num, SB.Index(Out_Num, "#", 1)+1, SB.Length(Out_Num)-1);
			SB.Replace_Slice(Done, 5-SB.Length(Out_Num)+1, 5, SB.To_String(Out_Num));
			return Done;

		exception
			when Constraint_Error => 
				Error_Number(SB.To_String(Input));
				return SB.To_Bounded_String("00000");
				
	end Get_Binary_5;


	function Get_Binary_16 (Input: in SB.Bounded_String) return SB.Bounded_String is
			Out_Num: SB.Bounded_String;
			Done: SB.Bounded_String:= SB.To_Bounded_String("0000000000000000");
			Temp: String:= "                                ";
			type Integer_16 is range 0..65535;
			Temp_Integer: Integer_16;

		begin
			Temp_Integer:= Integer_16'Value(SB.To_String(Input));
			I_IO.Put(To=>Temp, Item=>Integer(Temp_Integer), Base=>2);
			Out_Num:= SB.To_Bounded_String(Temp);
			Out_Num:= SB.Bounded_Slice(Out_Num, SB.Index(Out_Num, "#", 1)+1, SB.Length(Out_Num)-1);
			SB.Replace_Slice(Done, 16-SB.Length(Out_Num)+1, 16, SB.To_String(Out_Num));
			return Done;

		exception
			when Constraint_Error => 
				Error_Number(SB.To_String(Input));
				return SB.To_Bounded_String("0000000000000000");

	end Get_Binary_16;


	function Get_Binary_16_Signed (Input: in SB.Bounded_String) return SB.Bounded_String is
			Out_Num: SB.Bounded_String;
			Done: SB.Bounded_String:= SB.To_Bounded_String("0000000000000000");
			Temp: String:= "                                ";
			type Integer_16 is range -32768..32767;
			Temp_Integer: Integer_16;

		begin
			Temp_Integer:= Integer_16'Value(SB.To_String(Input));
			if Temp_Integer < 0 then
				Temp_Integer:= Integer_16(Integer(Temp_Integer)+32768);
				Done:= SB.To_Bounded_String("1000000000000000");
			end if;
			I_IO.Put(To=>Temp, Item=>Integer(Temp_Integer), Base=>2);
			Out_Num:= SB.To_Bounded_String(Temp);
			Out_Num:= SB.Bounded_Slice(Out_Num, SB.Index(Out_Num, "#", 1)+1, SB.Length(Out_Num)-1);
			SB.Replace_Slice(Done, 16-SB.Length(Out_Num)+1, 16, SB.To_String(Out_Num));
			return Done;

		exception
			when Constraint_Error => 
				Error_Number(SB.To_String(Input));
				return SB.To_Bounded_String("0000000000000000");

	end Get_Binary_16_Signed;


	function Get_Binary_26 (Input: in SB.Bounded_String) return SB.Bounded_String is
			Out_Num: SB.Bounded_String;
			Done: SB.Bounded_String:= SB.To_Bounded_String("00000000000000000000000000");
			Temp: String:= "                                ";
			type Integer_26 is range 0..268435455;
			Temp_Integer: Integer_26;

		begin
			Temp_Integer:= Integer_26'Value(SB.To_String(Input));
			if Temp_Integer mod 4 /= 0 then
				raise Constraint_Error;
			end if;
			Temp_Integer:= Temp_Integer/4;
			I_IO.Put(To=>Temp, Item=>Integer(Temp_Integer), Base=>2);
			Out_Num:= SB.To_Bounded_String(Temp);
			Out_Num:= SB.Bounded_Slice(Out_Num, SB.Index(Out_Num, "#", 1)+1, SB.Length(Out_Num)-1);
			SB.Replace_Slice(Done, 26-SB.Length(Out_Num)+1, 26, SB.To_String(Out_Num));
			return Done;

		exception
			when Constraint_Error => 
				Error_Number(SB.To_String(Input));
				return SB.To_Bounded_String("00000000000000000000000000");

	end Get_Binary_26;


	procedure Error_Register (Input: in String) is 
		begin
			Put(Positive'Image(Current_Line_Number));
			Put("::Register '");
			Put(Input);
			Put_Line("' not valid");
			Error_Flag:= True;

	end Error_Register;


	procedure Error_Number (Input: in String) is
		begin
			Put(Positive'Image(Current_Line_Number));
			Put("::Value '");
			Put(Input);
			Put_Line("' not valid");
			Error_Flag:= True;

	end Error_Number;


	procedure Error_Opcode (Input: in String) is
		begin
			Put(Positive'Image(Current_Line_Number));
			Put("::Op_Code '");
			Put(Input);
			Put_Line("' not valid");
			Error_Flag:= True;

	end Error_Opcode;

end Assemble_Functions;