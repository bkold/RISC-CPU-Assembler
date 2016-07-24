Package body Assemble_Functions is

	function Build (Source_File, Output_File: in out File_Type) return Boolean is
		Current_Line: SB.Bounded_String;
		Saved_Current_Line_Number: Positive:= Current_Line_Number;
		Saved_Instruction_Number: Natural:= Instruction_Number;
		begin
			Current_Line_Number:=1;			
			Instruction_Number:= 0;
			Error_Flag:= False;
			Get_Labels(Source_File);
			while not End_Of_File(Source_File) loop
				Current_Line:=Pull_Clean_Line(Source_File);	
				if SB.Length(Current_Line) > 0 --if anything is in the string
					and then SB.Element(Current_Line, SB.Index_Non_Blank(Current_Line)) /= '[' --and not a label line
						and then SB.Element(Current_Line, SB.Index_Non_Blank(Current_Line)) /= '-' then --and not a comment line
							SB_IO.Put_Line(Output_File, Assemble(Current_Line)); 
							Instruction_Number:= Instruction_Number+1;	 --increment for label arithmetic 				
				end if;	
				Current_Line_Number:=Current_Line_Number+1;			
			end loop;
			Current_Line_Number:=Saved_Current_Line_Number;
			Instruction_Number:= Saved_Instruction_Number;

			return Error_Flag;
	end Build;

	function Pull_Clean_Line(Source_File: in File_Type) return SB.Bounded_String is
		Line: SB.Bounded_String;
		Index: Natural;
		begin
			declare --resolve the case that the line is full of spaces, hiding the instruction
				Pulled_Line: String:= Get_Line(Source_File); --get entire line

				begin

					Index:= Ada.Strings.Fixed.Index(Pulled_Line, "--");
					if Index > 0 then
						Ada.Strings.Fixed.Delete(Pulled_Line, Index, Pulled_Line'Length); --delete the comment 
					end if;

					for I in Integer range 1..Pulled_Line'Length loop --loop to remove the tab character
						if Pulled_Line(I) = Tab then
							Pulled_Line(I):= ' ';
						end if;
					end loop;

					for I in Integer range 1..Pulled_Line'Length loop --remove consecutive spaces
						if Pulled_Line(I) = ' ' then
							Index:= Ada.Strings.Fixed.Index_Non_Blank(Pulled_Line, I);
							if Index > 0 then
								Ada.Strings.Fixed.Replace_Slice(Pulled_Line, I, Index-1, " ");
							end if;
						end if;
					end loop;

					Line:= SB.To_Bounded_String(Source=>Pulled_Line, Drop=>Ada.Strings.Right); --move into bounded string

					SB.Trim(Line, White_Space, White_Space); --remove the leading spaces and tabs
			end;			

			return Line;

	end Pull_Clean_Line;


	procedure Get_Labels (Source_File: in out File_Type) is
		Current_Line: SB.Bounded_String;
		Char: Character;
		Index: Natural;
		Saved_Current_Line_Number: Positive:= Current_Line_Number;
		Saved_Instruction_Number: Natural:= Instruction_Number;
		begin
			Current_Line_Number:=1;
			Instruction_Number:= 0;
			while not End_Of_File(Source_File) loop
				Current_Line:=Pull_Clean_Line(Source_File);
				Index:=SB.Index_Non_Blank(Current_Line);
				if Index > 0 then
					Char:= SB.Element(Current_Line, Index);
					if Char = '[' then
						--Label
						Add_Label(Current_Line, Instruction_Number);
					elsif Char /= '-' then
						--instruction
						Instruction_Number:= Instruction_Number+1;
					end if;
				end if;
				Current_Line_Number:=Current_Line_Number+1;
			end loop;
			Current_Line_Number:=Saved_Current_Line_Number;
			Instruction_Number:= Saved_Instruction_Number;

			Reset(Source_File);
			return;

	end Get_Labels;

	procedure Add_Label (Current_Line: in SB.Bounded_String; Instruction_Number: in Integer) is
		Line: SB.Bounded_String;
		begin	
			--Line is set to the string within the brackets
			Line:= SB.Bounded_Slice(
				Current_Line, 
				SB.Index(Current_Line, "[")+1,
				SB.Index(Current_Line, "]")-1);
			--adds Line to the Label_Tree
			if Imm_Trie.Add_String(Label_Tree, SB.To_String(Line), Instruction_Number) = False then
				Error_Label; --if an error occured
			end if;
			return;
		exception
			when others=> Error_Label;

	end Add_Label;


	function Assemble (Input: in SB.Bounded_String) return SB.Bounded_String is
		Op_Code: Op_Codes;
		Field_1: SB.Bounded_String;
		Field_2: SB.Bounded_String;
		Field_3: SB.Bounded_String;
		Field_Numbers: Field_Record:= (others=>0);
		Output_Number: Unsigned_32:= 0;
		Output: SB.Bounded_String;
		begin
			Get_Fields(Input, Op_Code, Field_1, Field_2, Field_3); --gets the individual fields
			Translate_Fields(Op_Code, Field_1, Field_2, Field_3, Field_Numbers); --converts the fields into binary
			Get_Specials(Op_Code, Field_Numbers); --gets the special fields for the designated op code

			--constructs the instruction
			case Op_Code is  
				when BAL_32 =>
					Output_Number:= 2#0000010000010001# * 2**16 + Field_Numbers.IMM;

				when BEQ_32 =>
					Output_Number:= 2#000011# * 2**26 + Field_Numbers.Field_1 * 2**21 + Field_Numbers.Field_2 * 2**16 + Field_Numbers.IMM;

				when BGEZ_32..BLEZAL_32 =>
					Output_Number:= 2#000001# * 2**26 + Field_Numbers.Field_1 * 2**21 + Field_Numbers.Special_1 * 2**16 + Field_Numbers.IMM;

				when J_32..SJAL_32 =>
					Output_Number:= Field_Numbers.Special_1 * 2**26 + Field_Numbers.IMM;

				when JALR_32..JR_32 =>
					Output_Number:= 2#000001# * 2**26 + Field_Numbers.Field_1 * 2**21 + Field_Numbers.Special_1 * 2**16;

				when ADD_32..XOR_32 =>
					Output_Number:= Field_Numbers.Field_2 * 2**21 + Field_Numbers.Field_3 * 2**16 + Field_Numbers.Field_1 * 2**11 + Field_Numbers.Special_2;

				when ADDI_32..SUBIU_32 =>
					Output_Number:= Field_Numbers.Special_1 * 2**26 + Field_Numbers.Field_2 * 2**21 + Field_Numbers.Field_1 * 2**16 + Field_Numbers.IMM;

				when LUI_32..SW_32 =>
					Output_Number:= Field_Numbers.Special_1 * 2**26 + Field_Numbers.Base * 2**21 + Field_Numbers.Field_1 * 2**16 + Field_Numbers.IMM;

				when BCPU_32 =>
					Output_Number:= 2#001100# * 2**26 + Field_Numbers.Field_1 * 2**21 + Field_Numbers.Field_2 * 2**16;

				when BCPUJ_32 =>
					Output_Number:= 2#001101# * 2**26 + Field_Numbers.IMM;

				when BCPUJR_32 => 
					Output_Number:= 2#001111# * 2**26 + Field_Numbers.Field_1 * 2**21;

				when EXIT_32 => 
					Output_Number:= 2#00100100000000000000000000000000#;

				when SLEEP_32 =>
					Output_Number:= 2#00100000000# * 2**21 + Field_Numbers.Field_2 * 2**16 + Field_Numbers.Field_1;

			end case;

			--format the number for output
			declare
				use SB; 
				Temp: String:= "                                             ";
				Temp_Bounded: Bounded_String;
				begin
					Output:= To_Bounded_String("32'b00000000000000000000000000000000");
					Mod_IO.Put(To=>Temp, Item=>Output_Number, Base=>2);
					Temp_Bounded:= To_Bounded_String(Temp, Drop=>Ada.Strings.Left);
					Temp_Bounded:= Bounded_Slice(Temp_Bounded, Index(Temp_Bounded, "#", 1)+1, Length(Temp_Bounded)-1);
					Output:= Replace_Slice(Output, Length(Output)-Length(Temp_Bounded)+1, Length(Output), To_String(Temp_Bounded));
					
			end;
			--SB_IO.Put_Line(Output);
			return Output;

	end Assemble;


	procedure Get_Fields(Input: in SB.Bounded_String; Op_Code: out Op_Codes; Field_1, Field_2, Field_3: out SB.Bounded_String) is
		use SB;
		Index_Start: Natural;
		Index_End: Natural;
		begin
			Index_Start:= Index_Non_Blank(Input, 1);
			Index_End:= Index(Input, " ", Index_Start);
			if Index_End = 0 then
				Op_Code:= Get_Op_Code(Bounded_Slice(Input, Index_Start, Length(Input)));
				return;
			end if;
			Op_Code:= Get_Op_Code(Bounded_Slice(Input, Index_Start, Index_End-1));

			Index_Start:= Index_Non_Blank(Input, Index_End+1);
			Index_End:= Index(Input, ",", Index_Start);
			if Index_End = 0 then
				Field_1:= Bounded_Slice(Input, Index_Start, Length(Input));
				return;
			end if;
			Field_1:= Bounded_Slice(Input, Index_Start, Index_End-1);

			Index_Start:= Index_Non_Blank(Input, Index_End+1);
			Index_End:= Index(Input, ",", Index_Start);
			if Index_End = 0 then
				Field_2:= Bounded_Slice(Input, Index_Start, Length(Input));
				return;
			end if;
			Field_2:= Bounded_Slice(Input, Index_Start, Index_End-1);

			Index_Start:= Index_Non_Blank(Input, Index_End+1);
			Index_End:= Index(Input, " ", Index_Start);
			if Index_End = 0 then
				Field_3:= Bounded_Slice(Input, Index_Start, Length(Input));
				return;
			end if;
			Field_3:= Bounded_Slice(Input, Index_Start, Index_End-1);

		exception 
			when others => return;

	end Get_Fields;


	procedure Translate_Fields (Op_Code: in Op_Codes; Field_1_String, Field_2_String, Field_3_String: in SB.Bounded_String; Field_Numbers: in out Field_Record) is
		use SB;
		Index_Start: Natural:= 1;
		Index_End: Natural:= 1;
		begin
			case Op_Code is  
				when BAL_32 =>
					if Length(Field_1_String) /= 0 and then Element(Field_1_String, 1) in Letter then --if Field is label
						Field_Numbers.IMM:= Get_Binary_16_Signed_Label(Field_1_String);
					else --it's a number
						Field_Numbers.IMM:= Get_Binary_16_Signed(Field_1_String);
					end if;

				when BEQ_32 =>
					Field_Numbers.Field_1:= Get_Register(Field_1_String);
					Field_Numbers.Field_2:= Get_Register(Field_2_String);
					if Length(Field_3_String) /= 0 and then Element(Field_3_String, 1) in Letter then
						Field_Numbers.IMM:= Get_Binary_16_Signed_Label(Field_3_String);
					else
						Field_Numbers.IMM:= Get_Binary_16_Signed(Field_3_String);
					end if;

				when BGEZ_32..BLEZAL_32 => 
					Field_Numbers.Field_1:= Get_Register(Field_1_String);
					if Length(Field_2_String) /= 0 and then Element(Field_2_String, 1) in Letter then
						Field_Numbers.IMM:= Get_Binary_16_Signed_Label(Field_2_String);
					else
						Field_Numbers.IMM:= Get_Binary_16_Signed(Field_2_String);
					end if;

				when J_32..SJAL_32 =>
					if Length(Field_1_String) /= 0 and then Element(Field_1_String, 1) in Letter then
						Field_Numbers.IMM:= Get_Binary_26_Label(Field_1_String);
					else
						Field_Numbers.IMM:= Get_Binary_26(Field_1_String);
					end if;

				when JALR_32..JR_32 =>
					Field_Numbers.Field_1:= Get_Register(Field_1_String);

				when ADD_32..XOR_32 =>
					Field_Numbers.Field_1:= Get_Register(Field_1_String);
					Field_Numbers.Field_2:= Get_Register(Field_2_String);
					Field_Numbers.Field_3:= Get_Register(Field_3_String);

				when ADDI_32..XORI_32 =>
					Field_Numbers.Field_1:= Get_Register(Field_1_String);
					Field_Numbers.Field_2:= Get_Register(Field_2_String);
					Field_Numbers.IMM:= Get_Binary_16(Field_3_String);

				when ADDIU_32..SUBIU_32 =>
					Field_Numbers.Field_1:= Get_Register(Field_1_String);
					Field_Numbers.Field_2:= Get_Register(Field_2_String);
					Field_Numbers.IMM:= Get_Binary_16_Signed(Field_3_String);

				when LUI_32 => 
					Field_Numbers.Field_1:= Get_Register(Field_1_String);
					Field_Numbers.IMM:= Get_Binary_16_Signed(Field_2_String);
					Field_Numbers.Base:= 0;

				when LW_32..SW_32 =>
					Field_Numbers.Field_1:= Get_Register(Field_1_String);
					Index_Start:= Index(Field_2_String, "(", 1);
					Index_End:= Index(Field_2_String, ")", Index_Start);
					Field_Numbers.Base:= Get_Binary_5(Bounded_Slice(Field_2_String, Index_Start+1, Index_End-1));
					Field_Numbers.IMM:= Get_Binary_16_Signed(Bounded_Slice(Field_2_String, 1, Index_Start-1));

				when BCPU_32 =>
					Field_Numbers.Field_1:= Get_Binary_5(Field_1_String);
					Field_Numbers.Field_2:= Get_Binary_5(Field_2_String);
						
				when BCPUJ_32 =>
					if Length(Field_1_String) /= 0 and then Element(Field_1_String, 1) in Letter then 
						Field_Numbers.IMM:= Get_Binary_26_Label(Field_1_String);
					else 
						Field_Numbers.IMM:= Get_Binary_26(Field_1_String);
					end if;

				when BCPUJR_32 => 
					Field_Numbers.Field_1:= Get_Register(Field_1_String);

				when SLEEP_32 =>
					Field_Numbers.Field_1:= Get_Binary_5(Field_1_String);
					Field_Numbers.Field_2:= Get_Binary_16(Field_2_String);

				when others =>
					return;

			end case;

	end Translate_Fields;


	procedure Get_Specials (Op_Code: in Op_Codes; Field_Numbers: in out Field_Record) is
		begin
			Field_Numbers.Special_1:= Specials_Array(1, Op_Codes'Pos(Op_Code));	
			Field_Numbers.Special_2:= Specials_Array(2, Op_Codes'Pos(Op_Code));	

	end Get_Specials;

	
	function Get_Op_Code (Input: in SB.Bounded_String) return Op_Codes is
		begin	
			return Op_Codes'Value(SB.To_String(Input));

		exception
			when Constraint_Error => 
				Error_Opcode(SB.To_String(Input));
				return EXIT_32;

	end Get_Op_Code;


	function Get_Register (Input: in SB.Bounded_String) return Unsigned_32 is
		Reg: Registers;
		begin
			if SB.Length(Input) = 0 then --throw soft error if no value sent
				Error_Missing_Operand;
				return 0;
			end if;
			Reg:= Registers'Value(SB.To_String(Input));
			return Unsigned_32(Registers'Pos(Reg));

		exception
			when Constraint_Error => --gets called if the value sent is not valid
				Error_Register(SB.To_String(Input));
				return 0;

	end Get_Register;


	function Get_Binary_5 (Input: in SB.Bounded_String) return Unsigned_32 is
		type Integer_5 is range 0..31;
		Temp_Integer: Integer_5;
		begin
			if SB.Length(Input) = 0 then
				Error_Missing_Operand;
				return 0;
			end if;
			Temp_Integer:= Integer_5'Value(SB.To_String(Input));
			return Unsigned_32(Temp_Integer);

		exception
			when Constraint_Error => --gets called when Temp_Integer is out of expected range
				Error_Number(SB.To_String(Input));
				return 0;
				
	end Get_Binary_5;


	function Get_Binary_16 (Input: in SB.Bounded_String) return Unsigned_32 is
		type Integer_16 is range 0..65535;
		Temp_Integer: Integer_16;
		begin
			if SB.Length(Input) = 0 then
				Error_Missing_Operand;
				return 0;
			end if;
			Temp_Integer:= Integer_16'Value(SB.To_String(Input));
			return Unsigned_32(Temp_Integer);

		exception
			when Constraint_Error => 
				Error_Number(SB.To_String(Input));
				return 0;

	end Get_Binary_16;


	function Get_Binary_16_Signed (Input: in SB.Bounded_String) return Unsigned_32 is
		type Integer_16 is range -32768..32767;
		Temp_Integer: Integer_16;
		begin
			if SB.Length(Input) = 0 then
				Error_Missing_Operand;
				return 0;
			end if;
			Temp_Integer:= Integer_16'Value(SB.To_String(Input));
			return (Unsigned_32(Temp_Integer) and 65535); --cut off the leading 1s

		exception
			when Constraint_Error => 
				Error_Number(SB.To_String(Input));
				return 0;

	end Get_Binary_16_Signed;


	function Get_Binary_26 (Input: in SB.Bounded_String) return Unsigned_32 is		
		type Integer_26 is range 0..67108863;
		Temp_Integer: Integer_26;
		begin
			if SB.Length(Input) = 0 then
				Error_Missing_Operand;
				return 0;
			end if;
			Temp_Integer:= Integer_26'Value(SB.To_String(Input));
			return Unsigned_32(Temp_Integer);

		exception
			when Constraint_Error => 
				Error_Number(SB.To_String(Input));
				return 0;
	end Get_Binary_26;


	function Get_Binary_16_Signed_Label (Input: in SB.Bounded_String) return Unsigned_32 is
		Label_Integer: Integer;
		begin
			Label_Integer:= Imm_Trie.Find_String(Label_Tree, SB.To_String(Input));
			if Label_Integer = -1 then --label is not found
				Error_Label;
				return 0;
			else	
				Label_Integer:= Label_Integer - (Instruction_Number + 1); --this is a relative branch
				return Get_Binary_16_Signed(SB.To_Bounded_String(Integer'Image(Label_Integer)));
			end if;
	end Get_Binary_16_Signed_Label;


	function Get_Binary_26_Label (Input: in SB.Bounded_String) return Unsigned_32 is
		Label_Integer: Integer;
		IMM: SB.Bounded_String;
		begin
			Label_Integer:= Imm_Trie.Find_String(Label_Tree, SB.To_String(Input));
			if Label_Integer = -1 then --label is not found
				Error_Label;
				return 0;
			end if;
							
			return Get_Binary_26(SB.To_Bounded_String(Integer'Image(Label_Integer)));
	end Get_Binary_26_Label;


	procedure Error_Register (Input: in String) is 
		begin
			Put(Positive'Image(Current_Line_Number));
			Put("::Register '");
			Put(Input);
			Put_Line("' not valid");
			Error_Flag:= True;

	end Error_Register;


	procedure Error_Missing_Operand is 
		begin
			Put(Positive'Image(Current_Line_Number));
			Put_Line("::Missing Operand");
			Error_Flag:= True;

	end Error_Missing_Operand;


	procedure Error_Label is 
		begin
			Put(Positive'Image(Current_Line_Number));
			Put_Line("::Label not valid");
			Error_Flag:= True;

	end Error_Label;


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