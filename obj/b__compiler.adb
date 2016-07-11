pragma Ada_95;
pragma Warnings (Off);
pragma Source_File_Name (ada_main, Spec_File_Name => "b__compiler.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__compiler.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E146 : Short_Integer; pragma Import (Ada, E146, "system__os_lib_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "ada__containers_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "ada__io_exceptions_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__strings_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings__maps_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings__maps__constants_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "ada__tags_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "ada__streams_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "interfaces__c_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "system__file_control_block_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "system__file_io_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__finalization_root_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "ada__finalization_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__storage_pools_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "system__finalization_masters_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "system__storage_pools__subpools_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "ada__calendar__time_zones_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "gnat__directory_operations_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "system__object_reader_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "system__dwarf_lines_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "system__pool_global_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__secondary_stack_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ada__strings__unbounded_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__directories_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "system__regexp_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "gnat__command_line_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "system__traceback__symbolic_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "ada__text_io_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "improved_trie_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "assemble_functions_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E159 := E159 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "assemble_functions__finalize_spec");
      begin
         F1;
      end;
      E179 := E179 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "improved_trie__finalize_spec");
      begin
         F2;
      end;
      E157 := E157 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "ada__text_io__finalize_spec");
      begin
         F3;
      end;
      E006 := E006 - 1;
      E151 := E151 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "system__regexp__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "ada__directories__finalize_spec");
      begin
         F5;
      end;
      E112 := E112 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__strings__unbounded__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__file_io__finalize_body");
      begin
         E144 := E144 - 1;
         F7;
      end;
      E124 := E124 - 1;
      E122 := E122 - 1;
      E184 := E184 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__pool_global__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__storage_pools__subpools__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__finalization_masters__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E025 := E025 + 1;
      Ada.Containers'Elab_Spec;
      E180 := E180 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E130 := E130 + 1;
      Ada.Strings'Elab_Spec;
      E053 := E053 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E059 := E059 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E129 := E129 + 1;
      Interfaces.C'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.File_Control_Block'Elab_Spec;
      E149 := E149 + 1;
      System.Finalization_Root'Elab_Spec;
      E132 := E132 + 1;
      Ada.Finalization'Elab_Spec;
      E127 := E127 + 1;
      System.Storage_Pools'Elab_Spec;
      E134 := E134 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E093 := E093 + 1;
      Gnat.Directory_Operations'Elab_Spec;
      System.Object_Reader'Elab_Spec;
      System.Dwarf_Lines'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E184 := E184 + 1;
      E122 := E122 + 1;
      System.Finalization_Masters'Elab_Body;
      E124 := E124 + 1;
      System.File_Io'Elab_Body;
      E144 := E144 + 1;
      E043 := E043 + 1;
      Ada.Tags'Elab_Body;
      E114 := E114 + 1;
      E055 := E055 + 1;
      System.Soft_Links'Elab_Body;
      E015 := E015 + 1;
      System.Os_Lib'Elab_Body;
      E146 := E146 + 1;
      System.Secondary_Stack'Elab_Body;
      E019 := E019 + 1;
      E048 := E048 + 1;
      E067 := E067 + 1;
      Gnat.Directory_Operations'Elab_Body;
      E193 := E193 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E112 := E112 + 1;
      Ada.Directories'Elab_Spec;
      System.Regexp'Elab_Spec;
      E151 := E151 + 1;
      Ada.Directories'Elab_Body;
      E006 := E006 + 1;
      Gnat.Command_Line'Elab_Spec;
      System.Traceback.Symbolic'Elab_Body;
      E038 := E038 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E157 := E157 + 1;
      Gnat.Command_Line'Elab_Body;
      E191 := E191 + 1;
      Improved_Trie'Elab_Spec;
      E179 := E179 + 1;
      Assemble_Functions'Elab_Spec;
      E159 := E159 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_compiler");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/brian/Desktop/Thesis/Assembler/obj/improved_trie.o
   --   /home/brian/Desktop/Thesis/Assembler/obj/assemble_functions.o
   --   /home/brian/Desktop/Thesis/Assembler/obj/compiler.o
   --   -L/home/brian/Desktop/Thesis/Assembler/obj/
   --   -L/home/brian/Desktop/Thesis/Assembler/obj/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/4.9.4/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
