with Ada.Strings.Fixed;
Package body Assemble_Functions is

	function Build (Source_File, Output_File : in out File_Type) return Boolean is
		use Ada.Strings.Fixed;
		Saved_Current_Line_Number : constant Positive := Current_Line_Number;
		Saved_Instruction_Number : constant Natural := Instruction_Number;
	begin
		Current_Line_Number:=1;			
		Instruction_Number := 0;
		Error_Flag := False;
		Get_Labels(Source_File);
		while not End_Of_File(Source_File) loop
			declare
				Current_Line : constant String := Pull_Clean_Line(Source_File);
				First_Index : Natural;
				begin
					First_Index := Index_Non_Blank(Current_Line);
					if First_Index > 0 --if anything is in the string
						and then Current_Line(First_Index) /= '[' then --and not a label line then
							SB_IO.Put_Line(Output_File, Assemble(Current_Line)); 
							Instruction_Number := Instruction_Number+1;	 --increment for label arithmetic 				
					end if;	
					Current_Line_Number:=Current_Line_Number+1;
			end;				
		end loop;
		Current_Line_Number:=Saved_Current_Line_Number;
		Instruction_Number := Saved_Instruction_Number;

		return Error_Flag;
			
	end Build;

	function Pull_Clean_Line(Source_File : in File_Type) return String is
		use Ada.Strings.Fixed;
		Char_Index : Natural;
		Pulled_Line : String := Get_Line(Source_File); --get entire line
	begin

		Char_Index := Index(Pulled_Line, "--");
		if Char_Index > 0 then
			Delete(Pulled_Line, Char_Index, Pulled_Line'Length); --delete the comment 
		end if;

		for I of Pulled_Line loop --loop to remove the tab character
			if I = Tab then
				I := ' ';
			end if;
		end loop;

		return Pulled_Line;

	end Pull_Clean_Line;


	procedure Get_Labels (Source_File : in out File_Type) is
		Saved_Current_Line_Number : constant Positive := Current_Line_Number;
		Saved_Instruction_Number : constant Natural := Instruction_Number;
	begin
		Current_Line_Number:=1;
		Instruction_Number := 0;
		while not End_Of_File(Source_File) loop
			declare
				Current_Line : constant String := Pull_Clean_Line(Source_File);
				First_Index : Natural;
				begin
					First_Index := Ada.Strings.Fixed.Index_Non_Blank(Current_Line);
					if First_Index > 0 then --if anything is in the string
						if Current_Line(First_Index) = '[' then --and not a label line then
							Add_Label(Current_Line, Instruction_Number);
						else
							Instruction_Number := Instruction_Number+1;	 --increment for label arithmetic 	
						end if;			
					end if;	
					Current_Line_Number:=Current_Line_Number+1;
			end;
		end loop;
		Current_Line_Number:=Saved_Current_Line_Number;
		Instruction_Number := Saved_Instruction_Number;
		Reset(Source_File);
		return;

	end Get_Labels;

	procedure Add_Label (Current_Line : in String; Instruction_Number : in Integer) is
		use Ada.Strings.Fixed;
		use SB;
		Line : SB.Bounded_String;
	begin	
		--Line is set to the string within the brackets
		Line := Bounded_Slice(
			To_Bounded_String(Current_Line), 
			Index(Current_Line, "[")+1,
			Index(Current_Line, "]")-1);
		Trim(Line, White_Space, White_Space);
		--adds Line to the Label_Tree
		if Imm_Trie.Add_String(Label_Tree, SB.To_String(Line), Instruction_Number) = False then
			Error_Label; --if an error occured
		end if;
		return;
	exception
		when Ada.Strings.Length_Error => Error_Length;
		when others => Error_Label;

	end Add_Label;


	function Assemble (Input : in String) return SB.Bounded_String is
		Op_Code : Op_Codes;
		Field_1: SB.Bounded_String;
		Field_2: SB.Bounded_String;
		Field_3: SB.Bounded_String;
		Field_Numbers : Field_Record := (others=>0);
		Output_Number : Unsigned_32 := 0;
		Output : SB.Bounded_String;
	begin
		Get_Fields(Input, Op_Code, Field_1, Field_2, Field_3); --gets the individual fields
		Translate_Fields(Op_Code, Field_1, Field_2, Field_3, Field_Numbers); --converts the fields into binary
		Get_Specials(Op_Code, Field_Numbers); --gets the special fields for the designated op code

		--constructs the instruction
		case Op_Code is  
			when BAL_32 =>
				Output_Number := 2#0000010000010001# * 2**16 + Field_Numbers.IMM;

			when BEQ_32 =>
				Output_Number := 2#000011# * 2**26 + Field_Numbers.Field_1 * 2**21 + Field_Numbers.Field_2 * 2**16 + Field_Numbers.IMM;
				
			when BGEZ_32..BLEZAL_32 =>
				Output_Number := 2#000001# * 2**26 + Field_Numbers.Field_1 * 2**21 + Field_Numbers.Special_1 * 2**16 + Field_Numbers.IMM;

			when J_32..SJAL_32 =>
				Output_Number := Field_Numbers.Special_1 * 2**26 + Field_Numbers.IMM;

			when JALR_32..JR_32 =>
				Output_Number := 2#000001# * 2**26 + Field_Numbers.Field_1 * 2**21 + Field_Numbers.Special_1 * 2**16;

			when ADD_32..XOR_32 =>
				Output_Number := Field_Numbers.Field_2 * 2**21 + Field_Numbers.Field_3 * 2**16 + Field_Numbers.Field_1 * 2**11 + Field_Numbers.Special_2;

			when ADDI_32..SUBIU_32 =>
				Output_Number := Field_Numbers.Special_1 * 2**26 + Field_Numbers.Field_2 * 2**21 + Field_Numbers.Field_1 * 2**16 + Field_Numbers.IMM;

			when LUI_32..SW_32 =>
				Output_Number := Field_Numbers.Special_1 * 2**26 + Field_Numbers.Base * 2**21 + Field_Numbers.Field_1 * 2**16 + Field_Numbers.IMM;

			when BCPU_32 =>
				Output_Number := 2#001100# * 2**26 + Field_Numbers.Field_1 * 2**21 + Field_Numbers.Field_2 * 2**16;

			when BCPUJ_32 =>
				Output_Number := 2#001101# * 2**26 + Field_Numbers.IMM;

			when BCPUJR_32 => 
				Output_Number := 2#001111# * 2**26 + Field_Numbers.Field_1 * 2**21;

			when EXIT_32 => 
				Output_Number := 2#00100100000000000000000000000000#;

			when SLEEP_32 =>
				Output_Number := 2#00100000000# * 2**21 + Field_Numbers.Field_2 * 2**16 + Field_Numbers.Field_1;

		end case;

		--format the number for output
		declare
			use SB; 
			Temp : String := "                                             ";
			Temp_Bounded : Bounded_String;
			begin
				Output := To_Bounded_String("32'b00000000000000000000000000000000");
				Mod_IO.Put(To=>Temp, Item=>Output_Number, Base=>2);
				Temp_Bounded := To_Bounded_String(Temp, Drop=>Ada.Strings.Left);
				Temp_Bounded := Bounded_Slice(Temp_Bounded, Index(Temp_Bounded, "#", 1)+1, Length(Temp_Bounded)-1);
				Output := Replace_Slice(Output, Length(Output)-Length(Temp_Bounded)+1, Length(Output), To_String(Temp_Bounded));
				
		end;
		--SB_IO.Put_Line(Output);
		return Output;

	end Assemble;


	procedure Get_Fields(Input : in String; Op_Code : out Op_Codes; Field_1, Field_2, Field_3: out SB.Bounded_String) is
		use SB;
		use Ada.Strings.Fixed;
		Index_Start : Natural;
		Index_End : Natural;
	begin
		Index_Start := Index_Non_Blank(Input, 1);
		Index_End := Index(Input, " ", Index_Start);
		if Index_End = 0 then
			Op_Code := Get_Op_Code(Input(Index_Start..Input'Last));
			goto Exit_Get_Fields;
		end if;
		Op_Code := Get_Op_Code(Input(Index_Start..Index_End-1));

		Index_Start := Index_Non_Blank(Input, Index_End+1);
		Index_End := Index(Input, ",", Index_Start);
		if Index_End = 0 or Index_End = Input'Last then
			Field_1 := To_Bounded_String(Input(Index_Start..Input'Last));
			goto Exit_Get_Fields;
		end if;
		Field_1 := To_Bounded_String(Input(Index_Start..Index_End-1));

		Index_Start := Index_Non_Blank(Input, Index_End+1);
		Index_End := Index(Input, ",", Index_Start);
		if Index_End = 0 or Index_End = Input'Last then
			Field_2 := To_Bounded_String(Input(Index_Start..Input'Last));
			goto Exit_Get_Fields;
		end if;
		Field_2 := To_Bounded_String(Input(Index_Start..Index_End-1));

		Index_Start := Index_Non_Blank(Input, Index_End+1);
		Index_End := Index(Input, " ", Index_Start);
		if Index_End = 0 or Index_End = Input'Last then
			Field_3 := To_Bounded_String(Input(Index_Start..Input'Last));
			goto Exit_Get_Fields;
		end if;
		Field_3 := To_Bounded_String(Input(Index_Start..Index_End-1));

		--Check if we left something behind
		Index_Start := Index_End;
		Index_End := Index_Non_Blank(Input, Index_Start+1);
		if Index_End /= 0 then
			Error_Unknown(Input(Index_Start..Input'Last));
		end if;

		<<Exit_Get_Fields>>
		return;
	exception
		when Constraint_Error => return; --this will trigger when we have trailing spaces

		when Ada.Strings.Length_Error => --The substring is too big
			Error_Length; 
			Op_Code:=EXIT_32;
			
	end Get_Fields;


	procedure Translate_Fields (Op_Code : in Op_Codes; Field_1_String, Field_2_String, Field_3_String : in SB.Bounded_String; Field_Numbers : in out Field_Record) is
		use SB;
		Index_Start : Natural := 1;
		Index_End : Natural := 1;
	begin
		case Op_Code is  
			when BAL_32 =>
				if Length(Field_1_String) /= 0 and then Element(Field_1_String, 1) in Letter then --if Field is label
					Field_Numbers.IMM := Get_Binary_Label(Field_1_String, Signed_Bits_16);
				else --it's a number
					Field_Numbers.IMM := Get_Binary(Field_1_String, Signed_Bits_16);
				end if;

			when BEQ_32 =>
				Field_Numbers.Field_1 := Get_Register(Field_1_String);
				Field_Numbers.Field_2 := Get_Register(Field_2_String);
				if Length(Field_3_String) /= 0 and then Element(Field_3_String, 1) in Letter then
					Field_Numbers.IMM := Get_Binary_Label(Field_3_String, Signed_Bits_16);
				else
					Field_Numbers.IMM := Get_Binary(Field_3_String, Signed_Bits_16);
				end if;

			when BGEZ_32..BLEZAL_32 => 
				Field_Numbers.Field_1 := Get_Register(Field_1_String);
				if Length(Field_2_String) /= 0 and then Element(Field_2_String, 1) in Letter then
					Field_Numbers.IMM := Get_Binary_Label(Field_2_String, Signed_Bits_16);
				else
					Field_Numbers.IMM := Get_Binary(Field_2_String, Signed_Bits_16);
				end if;

			when J_32..SJAL_32 =>
				if Length(Field_1_String) /= 0 and then Element(Field_1_String, 1) in Letter then
					Field_Numbers.IMM := Get_Binary_Label(Field_1_String, Bits_26);
				else
					Field_Numbers.IMM := Get_Binary(Field_1_String, Bits_26);
				end if;

			when JALR_32..JR_32 =>
				Field_Numbers.Field_1 := Get_Register(Field_1_String);

			when ADD_32..XOR_32 =>
				Field_Numbers.Field_1 := Get_Register(Field_1_String);
				Field_Numbers.Field_2 := Get_Register(Field_2_String);
				Field_Numbers.Field_3 := Get_Register(Field_3_String);

			when ADDI_32..XORI_32 =>
				Field_Numbers.Field_1 := Get_Register(Field_1_String);
				Field_Numbers.Field_2 := Get_Register(Field_2_String);
				Field_Numbers.IMM := Get_Binary(Field_3_String, Bits_16);

			when ADDIU_32..SUBIU_32 =>
				Field_Numbers.Field_1 := Get_Register(Field_1_String);
				Field_Numbers.Field_2 := Get_Register(Field_2_String);
				Field_Numbers.IMM := Get_Binary(Field_3_String, Signed_Bits_16);

			when LUI_32 => 
				Field_Numbers.Field_1 := Get_Register(Field_1_String);
				Field_Numbers.IMM := Get_Binary(Field_2_String, Signed_Bits_16);
				Field_Numbers.Base := 0;

			when LW_32..SW_32 =>
				Field_Numbers.Field_1 := Get_Register(Field_1_String);
				Index_Start := Index(Field_2_String, "(", 1);
				Index_End := Index(Field_2_String, ")", Index_Start);
				Field_Numbers.Base := Get_Binary(Bounded_Slice(Field_2_String, Index_Start+1, Index_End-1), Bits_5);
				Field_Numbers.IMM := Get_Binary(Bounded_Slice(Field_2_String, 1, Index_Start-1), Signed_Bits_16);

			when BCPU_32 =>
				Field_Numbers.Field_1 := Get_Binary(Field_1_String, Bits_5);
				Field_Numbers.Field_2 := Get_Binary(Field_2_String, Bits_5);
					
			when BCPUJ_32 =>
				if Length(Field_1_String) /= 0 and then Element(Field_1_String, 1) in Letter then 
					Field_Numbers.IMM := Get_Binary_Label(Field_1_String, Bits_26);
				else 
					Field_Numbers.IMM := Get_Binary(Field_1_String, Bits_26);
				end if;

			when BCPUJR_32 => 
				Field_Numbers.Field_1 := Get_Register(Field_1_String);

			when SLEEP_32 =>
				Field_Numbers.Field_1 := Get_Binary(Field_1_String, Bits_5);
				Field_Numbers.Field_2 := Get_Binary(Field_2_String, Bits_16);

			when others =>
				return;

		end case;

	end Translate_Fields;


	procedure Get_Specials (Op_Code : in Op_Codes; Field_Numbers : in out Field_Record) is
	begin
		Field_Numbers.Special_1 := Specials_Array(1, Op_Codes'Pos(Op_Code));	
		Field_Numbers.Special_2 := Specials_Array(2, Op_Codes'Pos(Op_Code));	

	end Get_Specials;

	
	function Get_Op_Code (Input : in String) return Op_Codes is
	begin	
		return Op_Codes'Value(Input);

	exception
		when Constraint_Error => 
			Error_Opcode(Input);
			return EXIT_32;

	end Get_Op_Code;


	function Get_Register (Input : in SB.Bounded_String) return Unsigned_32 is
		Reg : Registers;
	begin
		if SB.Length(Input) = 0 then --throw soft error if no value sent
			Error_Missing_Operand;
			return 0;
		end if;
		Reg := Registers'Value(SB.To_String(Input));
		return Unsigned_32(Registers'Pos(Reg));

	exception
		when Constraint_Error => --gets called if the value sent is not valid
			Error_Register(SB.To_String(Input));
			return 0;

	end Get_Register;


	function Get_Binary (Input : in SB.Bounded_String; Length : in Valid_Binary) return Unsigned_32 is
		subtype Integer_5 is Integer range 0..31;
		subtype Integer_16 is Integer range 0..65535;
		subtype Integer_16_Signed is Integer range -32768..32767;
		subtype Integer_26 is Integer range 0..67108863;
		Temp_Integer : Integer;
	begin
		if SB.Length(Input) = 0 then
			Error_Missing_Operand;
			return 0;
		end if;
		Temp_Integer := Integer'Value(SB.To_String(Input));

		case Length is
			when Bits_5 => 
				if Temp_Integer not in Integer_5 then
					goto Not_In_Range;
				end if;
			when Bits_16 => 
				if Temp_Integer not in Integer_16 then
					goto Not_In_Range;
				end if;
			when Signed_Bits_16 => 
				if Temp_Integer not in Integer_16_Signed then
					goto Not_In_Range;
				end if;
			when Bits_26 => 
				if Temp_Integer not in Integer_26 then
					goto Not_In_Range;
				end if;
		end case;

		return Unsigned_32(Temp_Integer);

		<<Not_In_Range>>
		Error_Number(SB.To_String(Input));
		return 0;

	exception
		when Constraint_Error => 
			Error_Number(SB.To_String(Input));
			return 0;

	end Get_Binary;


	function Get_Binary_Label (Input : in SB.Bounded_String; Length : in Valid_Binary_Labels) return Unsigned_32 is
		Label_Integer : Integer;
		Temp_Input : SB.Bounded_String;
	begin
		Temp_Input := SB.Trim(Input, White_Space, White_Space);
		Label_Integer := Imm_Trie.Find_String(Label_Tree, SB.To_String(Temp_Input));
		if Label_Integer = -1 then --label is not found
			Error_Label;
			return 0;
		end if;					

		if Length = Signed_Bits_16 then
			Label_Integer := Label_Integer - (Instruction_Number + 1); --this is a relative branch		
		end if;

		return Get_Binary(SB.To_Bounded_String(Integer'Image(Label_Integer)), Length);

	end Get_Binary_Label;


	procedure Error_Register (Input : in String) is 
	begin
		Put_Line("Error ->" & Positive'Image(Current_Line_Number) & " :: Register '" & Input & "' not valid");
		Error_Flag := True;

	end Error_Register;


	procedure Error_Missing_Operand is 
	begin
		Put_Line("Error ->" & Positive'Image(Current_Line_Number) & " :: Missing Operand");
		Error_Flag := True;

	end Error_Missing_Operand;


	procedure Error_Label is 
	begin
		Put_Line("Error ->" & Positive'Image(Current_Line_Number) & " :: Label not valid");
		Error_Flag := True;

	end Error_Label;


	procedure Error_Length is 
	begin
		Put_Line("Error ->" & Positive'Image(Current_Line_Number) & " :: Argument can't exceed 36 characters");
		Error_Flag := True;

	end Error_Length;


	procedure Error_Number (Input : in String) is
	begin
		Put_Line("Error ->" & Positive'Image(Current_Line_Number) & " :: Value '" & Input & "' not valid");
		Error_Flag := True;

	end Error_Number;


	procedure Error_Opcode (Input : in String) is
	begin
		Put_Line("Error ->" & Positive'Image(Current_Line_Number) & " :: Op_Code '" & Input & "' not valid");
		Error_Flag := True;

	end Error_Opcode;


	procedure Error_Unknown (Input : in String) is
	begin
		Put_Line("Error ->" & Positive'Image(Current_Line_Number) & " :: Unknown String '" & Input & "'");
		Error_Flag := True;

	end Error_Unknown;

end Assemble_Functions;