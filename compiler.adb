with Ada.Text_IO;
with Ada.Command_Line;
with Assemble_Functions;
with Ada.Directories;
use Ada.Command_Line;
use Ada.Text_IO;

--main function controls reading and writing
--assemble package handles each line

procedure Compiler is
		Mode: Integer;
		Source_File: File_Type;
		Output_File: File_Type;	
		Error_Flag: Boolean;
	begin
		if Argument(1) = "-d" then --disassemble
			Mode:=1;
		elsif Argument(1) = "-a" then --assemble
			Mode:=2;
		end if;

		--open files
		Open(Source_File, In_File, Argument(2));
		Create(File=>Output_File, Name=>"~Out.s");

		if Mode = 2 then
			Error_Flag:= Assemble_Functions.Build(Source_File, Output_File);
		elsif Mode = 1 then
			Put_Line("Disassemble is not supported yet");
		end if;

		Close(Source_File);
		Close(Output_File);

		if Error_Flag = False then
			if Argument_Count > 2 then
				if Ada.Directories.Exists(Argument(3)) then
					Ada.Directories.Delete_File(Argument(3));
				end if;
				Ada.Directories.Rename("~Out.s", Argument(3));
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