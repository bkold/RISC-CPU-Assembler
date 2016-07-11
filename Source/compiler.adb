with Ada.Text_IO;
with Assemble_Functions;
with Ada.Directories;
with GNAT.Command_Line;
with Ada.Strings.Bounded;
use GNAT.Command_Line;
use Ada.Text_IO;

--main function controls reading and writing
--assemble package handles each line

procedure Compiler is
		package B_S is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 50);
		Mode: Integer:= 0;
		Source_File: File_Type;
		Output_File: File_Type;	
		Error_Flag: Boolean:= True;
		Output_File_Name: B_S.Bounded_String;
		Input_File_Name: B_S.Bounded_String;
	begin
		loop
			case Getopt ("a d  o:") is
				when 'a' =>
					if Mode = 0 then
						Mode:=2;
					else
						Put_Line("Conflicting agument: '-a'");
						return;
					end if;
				when 'd' =>
					if Mode = 0 then
						Mode:=1;
					else
						Put_Line("Conflicting agument: '-d'");
						return;
					end if;
				when 'o' =>
					Output_File_Name:= B_S.To_Bounded_String(Parameter);
				when others =>
					exit;
			end case;
		end loop;
		
		if Mode = 0 then
			Mode:= 2; --default to Mode 2
		end if;

		Input_File_Name:= B_S.To_Bounded_String(Get_Argument);

		if B_S.Length(Input_File_Name) = 0 then
			Put_Line("No file name given");
			return;
		end if;
		--open files
		Open(Source_File, In_File, B_S.To_String(Input_File_Name));
		Create(File=>Output_File, Name=>"~Out.s");

		if Mode = 2 then
			Error_Flag:= Assemble_Functions.Build(Source_File, Output_File);
		elsif Mode = 1 then
			Put_Line("Disassemble is not supported yet");
		end if;

		Close(Source_File);
		Close(Output_File);

		if Error_Flag = False then
			if B_S.Length(Output_File_Name) > 0 then
				if Ada.Directories.Exists(B_S.To_String(Output_File_Name)) then
					Ada.Directories.Delete_File(B_S.To_String(Output_File_Name));
				end if;
				Ada.Directories.Rename("~Out.s", B_S.To_String(Output_File_Name));
			else
				if Ada.Directories.Exists("Out.s") then
					Ada.Directories.Delete_File("Out.s");
				end if;
				Ada.Directories.Rename("~Out.s", "Out.s");
			end if;
		else
			Ada.Directories.Delete_File("~Out.s");
		end if;

end Compiler;
