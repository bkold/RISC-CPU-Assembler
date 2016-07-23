pragma Ada_95;
pragma Warnings (Off);
with System;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2016 (20160515-49)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_compiler" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#fcb6df3c#;
   pragma Export (C, u00001, "compilerB");
   u00002 : constant Version_32 := 16#b6df930e#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#937076cc#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#ca2335cb#;
   pragma Export (C, u00005, "ada__directoriesB");
   u00006 : constant Version_32 := 16#eb9f206b#;
   pragma Export (C, u00006, "ada__directoriesS");
   u00007 : constant Version_32 := 16#c5dcd3d2#;
   pragma Export (C, u00007, "ada__calendarB");
   u00008 : constant Version_32 := 16#12a38fcc#;
   pragma Export (C, u00008, "ada__calendarS");
   u00009 : constant Version_32 := 16#e7214354#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#020f9e08#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#e947e6a9#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerB");
   u00012 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerS");
   u00013 : constant Version_32 := 16#6326c08a#;
   pragma Export (C, u00013, "systemS");
   u00014 : constant Version_32 := 16#5f84b5ab#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#fda218df#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00016, "system__parametersB");
   u00017 : constant Version_32 := 16#1d0ccdf5#;
   pragma Export (C, u00017, "system__parametersS");
   u00018 : constant Version_32 := 16#0f0cb66d#;
   pragma Export (C, u00018, "system__secondary_stackB");
   u00019 : constant Version_32 := 16#c8470fe3#;
   pragma Export (C, u00019, "system__secondary_stackS");
   u00020 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00020, "system__storage_elementsB");
   u00021 : constant Version_32 := 16#4ee58a8e#;
   pragma Export (C, u00021, "system__storage_elementsS");
   u00022 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00022, "system__stack_checkingB");
   u00023 : constant Version_32 := 16#ed99ab62#;
   pragma Export (C, u00023, "system__stack_checkingS");
   u00024 : constant Version_32 := 16#87a448ff#;
   pragma Export (C, u00024, "system__exception_tableB");
   u00025 : constant Version_32 := 16#3e88a9c8#;
   pragma Export (C, u00025, "system__exception_tableS");
   u00026 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00026, "system__exceptionsB");
   u00027 : constant Version_32 := 16#0b45ad7c#;
   pragma Export (C, u00027, "system__exceptionsS");
   u00028 : constant Version_32 := 16#4c9e814d#;
   pragma Export (C, u00028, "system__exceptions__machineS");
   u00029 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00029, "system__exceptions_debugB");
   u00030 : constant Version_32 := 16#1dac394e#;
   pragma Export (C, u00030, "system__exceptions_debugS");
   u00031 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00031, "system__img_intB");
   u00032 : constant Version_32 := 16#61fd2048#;
   pragma Export (C, u00032, "system__img_intS");
   u00033 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00033, "system__tracebackB");
   u00034 : constant Version_32 := 16#3d041e4e#;
   pragma Export (C, u00034, "system__tracebackS");
   u00035 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00035, "system__traceback_entriesB");
   u00036 : constant Version_32 := 16#637d36fa#;
   pragma Export (C, u00036, "system__traceback_entriesS");
   u00037 : constant Version_32 := 16#0162f862#;
   pragma Export (C, u00037, "system__traceback__symbolicB");
   u00038 : constant Version_32 := 16#dd19f67a#;
   pragma Export (C, u00038, "system__traceback__symbolicS");
   u00039 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00039, "ada__exceptions__tracebackB");
   u00040 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00040, "ada__exceptions__tracebackS");
   u00041 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00041, "interfacesS");
   u00042 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00042, "interfaces__cB");
   u00043 : constant Version_32 := 16#70be4e8c#;
   pragma Export (C, u00043, "interfaces__cS");
   u00044 : constant Version_32 := 16#5f72f755#;
   pragma Export (C, u00044, "system__address_operationsB");
   u00045 : constant Version_32 := 16#702a7eb9#;
   pragma Export (C, u00045, "system__address_operationsS");
   u00046 : constant Version_32 := 16#13b71684#;
   pragma Export (C, u00046, "system__crtlS");
   u00047 : constant Version_32 := 16#f82008fb#;
   pragma Export (C, u00047, "system__dwarf_linesB");
   u00048 : constant Version_32 := 16#0aa7ccc7#;
   pragma Export (C, u00048, "system__dwarf_linesS");
   u00049 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00049, "ada__charactersS");
   u00050 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00050, "ada__characters__handlingB");
   u00051 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00051, "ada__characters__handlingS");
   u00052 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00052, "ada__characters__latin_1S");
   u00053 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00053, "ada__stringsS");
   u00054 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00054, "ada__strings__mapsB");
   u00055 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00055, "ada__strings__mapsS");
   u00056 : constant Version_32 := 16#04ec3c16#;
   pragma Export (C, u00056, "system__bit_opsB");
   u00057 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00057, "system__bit_opsS");
   u00058 : constant Version_32 := 16#57a0bc09#;
   pragma Export (C, u00058, "system__unsigned_typesS");
   u00059 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00059, "ada__strings__maps__constantsS");
   u00060 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00060, "system__address_imageB");
   u00061 : constant Version_32 := 16#c2ca5db0#;
   pragma Export (C, u00061, "system__address_imageS");
   u00062 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00062, "system__img_unsB");
   u00063 : constant Version_32 := 16#c85480fe#;
   pragma Export (C, u00063, "system__img_unsS");
   u00064 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00064, "system__ioB");
   u00065 : constant Version_32 := 16#fd6437c5#;
   pragma Export (C, u00065, "system__ioS");
   u00066 : constant Version_32 := 16#cf909744#;
   pragma Export (C, u00066, "system__object_readerB");
   u00067 : constant Version_32 := 16#27c18a1d#;
   pragma Export (C, u00067, "system__object_readerS");
   u00068 : constant Version_32 := 16#1a74a354#;
   pragma Export (C, u00068, "system__val_lliB");
   u00069 : constant Version_32 := 16#f902262a#;
   pragma Export (C, u00069, "system__val_lliS");
   u00070 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00070, "system__val_lluB");
   u00071 : constant Version_32 := 16#2d52eb7b#;
   pragma Export (C, u00071, "system__val_lluS");
   u00072 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00072, "system__val_utilB");
   u00073 : constant Version_32 := 16#cf867674#;
   pragma Export (C, u00073, "system__val_utilS");
   u00074 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00074, "system__case_utilB");
   u00075 : constant Version_32 := 16#472fa95d#;
   pragma Export (C, u00075, "system__case_utilS");
   u00076 : constant Version_32 := 16#84a27f0d#;
   pragma Export (C, u00076, "interfaces__c_streamsB");
   u00077 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00077, "interfaces__c_streamsS");
   u00078 : constant Version_32 := 16#931ff6be#;
   pragma Export (C, u00078, "system__exception_tracesB");
   u00079 : constant Version_32 := 16#47f9e010#;
   pragma Export (C, u00079, "system__exception_tracesS");
   u00080 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00080, "system__wch_conB");
   u00081 : constant Version_32 := 16#785be258#;
   pragma Export (C, u00081, "system__wch_conS");
   u00082 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00082, "system__wch_stwB");
   u00083 : constant Version_32 := 16#554ace59#;
   pragma Export (C, u00083, "system__wch_stwS");
   u00084 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00084, "system__wch_cnvB");
   u00085 : constant Version_32 := 16#77ec58ab#;
   pragma Export (C, u00085, "system__wch_cnvS");
   u00086 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00086, "system__wch_jisB");
   u00087 : constant Version_32 := 16#f79c418a#;
   pragma Export (C, u00087, "system__wch_jisS");
   u00088 : constant Version_32 := 16#d083f760#;
   pragma Export (C, u00088, "system__os_primitivesB");
   u00089 : constant Version_32 := 16#e9a9d1fc#;
   pragma Export (C, u00089, "system__os_primitivesS");
   u00090 : constant Version_32 := 16#8f218b8f#;
   pragma Export (C, u00090, "ada__calendar__formattingB");
   u00091 : constant Version_32 := 16#67ade573#;
   pragma Export (C, u00091, "ada__calendar__formattingS");
   u00092 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00092, "ada__calendar__time_zonesB");
   u00093 : constant Version_32 := 16#6dc27f8f#;
   pragma Export (C, u00093, "ada__calendar__time_zonesS");
   u00094 : constant Version_32 := 16#d763507a#;
   pragma Export (C, u00094, "system__val_intB");
   u00095 : constant Version_32 := 16#2b83eab5#;
   pragma Export (C, u00095, "system__val_intS");
   u00096 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00096, "system__val_unsB");
   u00097 : constant Version_32 := 16#47085132#;
   pragma Export (C, u00097, "system__val_unsS");
   u00098 : constant Version_32 := 16#faa9a7b2#;
   pragma Export (C, u00098, "system__val_realB");
   u00099 : constant Version_32 := 16#9d0fb79b#;
   pragma Export (C, u00099, "system__val_realS");
   u00100 : constant Version_32 := 16#6c05c057#;
   pragma Export (C, u00100, "system__exn_llfB");
   u00101 : constant Version_32 := 16#df587b56#;
   pragma Export (C, u00101, "system__exn_llfS");
   u00102 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00102, "system__float_controlB");
   u00103 : constant Version_32 := 16#83da83b6#;
   pragma Export (C, u00103, "system__float_controlS");
   u00104 : constant Version_32 := 16#3356a6fd#;
   pragma Export (C, u00104, "system__powten_tableS");
   u00105 : constant Version_32 := 16#ab4ad33a#;
   pragma Export (C, u00105, "ada__directories__validityB");
   u00106 : constant Version_32 := 16#d34bdf62#;
   pragma Export (C, u00106, "ada__directories__validityS");
   u00107 : constant Version_32 := 16#e5480ede#;
   pragma Export (C, u00107, "ada__strings__fixedB");
   u00108 : constant Version_32 := 16#a86b22b3#;
   pragma Export (C, u00108, "ada__strings__fixedS");
   u00109 : constant Version_32 := 16#45c9251c#;
   pragma Export (C, u00109, "ada__strings__searchB");
   u00110 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00110, "ada__strings__searchS");
   u00111 : constant Version_32 := 16#5130abd7#;
   pragma Export (C, u00111, "ada__strings__unboundedB");
   u00112 : constant Version_32 := 16#4c956ffe#;
   pragma Export (C, u00112, "ada__strings__unboundedS");
   u00113 : constant Version_32 := 16#920eada5#;
   pragma Export (C, u00113, "ada__tagsB");
   u00114 : constant Version_32 := 16#13ca27f3#;
   pragma Export (C, u00114, "ada__tagsS");
   u00115 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00115, "system__htableB");
   u00116 : constant Version_32 := 16#e7e47360#;
   pragma Export (C, u00116, "system__htableS");
   u00117 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00117, "system__string_hashB");
   u00118 : constant Version_32 := 16#45ba181e#;
   pragma Export (C, u00118, "system__string_hashS");
   u00119 : constant Version_32 := 16#5b9edcc4#;
   pragma Export (C, u00119, "system__compare_array_unsigned_8B");
   u00120 : constant Version_32 := 16#ca25b107#;
   pragma Export (C, u00120, "system__compare_array_unsigned_8S");
   u00121 : constant Version_32 := 16#6a86c9a5#;
   pragma Export (C, u00121, "system__storage_pools__subpoolsB");
   u00122 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00122, "system__storage_pools__subpoolsS");
   u00123 : constant Version_32 := 16#6abe5dbe#;
   pragma Export (C, u00123, "system__finalization_mastersB");
   u00124 : constant Version_32 := 16#38daf940#;
   pragma Export (C, u00124, "system__finalization_mastersS");
   u00125 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00125, "system__img_boolB");
   u00126 : constant Version_32 := 16#96ffb161#;
   pragma Export (C, u00126, "system__img_boolS");
   u00127 : constant Version_32 := 16#cf417de3#;
   pragma Export (C, u00127, "ada__finalizationS");
   u00128 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00128, "ada__streamsB");
   u00129 : constant Version_32 := 16#2e6701ab#;
   pragma Export (C, u00129, "ada__streamsS");
   u00130 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00130, "ada__io_exceptionsS");
   u00131 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00131, "system__finalization_rootB");
   u00132 : constant Version_32 := 16#2cd4b31a#;
   pragma Export (C, u00132, "system__finalization_rootS");
   u00133 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00133, "system__storage_poolsB");
   u00134 : constant Version_32 := 16#40cb5e27#;
   pragma Export (C, u00134, "system__storage_poolsS");
   u00135 : constant Version_32 := 16#9aad1ff1#;
   pragma Export (C, u00135, "system__storage_pools__subpools__finalizationB");
   u00136 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00136, "system__storage_pools__subpools__finalizationS");
   u00137 : constant Version_32 := 16#020a3f4d#;
   pragma Export (C, u00137, "system__atomic_countersB");
   u00138 : constant Version_32 := 16#d77aed07#;
   pragma Export (C, u00138, "system__atomic_countersS");
   u00139 : constant Version_32 := 16#f4e1c091#;
   pragma Export (C, u00139, "system__stream_attributesB");
   u00140 : constant Version_32 := 16#8bc30a4e#;
   pragma Export (C, u00140, "system__stream_attributesS");
   u00141 : constant Version_32 := 16#064e8312#;
   pragma Export (C, u00141, "system__file_attributesS");
   u00142 : constant Version_32 := 16#7dc03a19#;
   pragma Export (C, u00142, "system__os_constantsS");
   u00143 : constant Version_32 := 16#b29d05bd#;
   pragma Export (C, u00143, "system__file_ioB");
   u00144 : constant Version_32 := 16#c45721ef#;
   pragma Export (C, u00144, "system__file_ioS");
   u00145 : constant Version_32 := 16#d3560627#;
   pragma Export (C, u00145, "system__os_libB");
   u00146 : constant Version_32 := 16#bf5ce13f#;
   pragma Export (C, u00146, "system__os_libS");
   u00147 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00147, "system__stringsB");
   u00148 : constant Version_32 := 16#1d99d1ec#;
   pragma Export (C, u00148, "system__stringsS");
   u00149 : constant Version_32 := 16#9eb95a22#;
   pragma Export (C, u00149, "system__file_control_blockS");
   u00150 : constant Version_32 := 16#933fac2f#;
   pragma Export (C, u00150, "system__regexpB");
   u00151 : constant Version_32 := 16#40146746#;
   pragma Export (C, u00151, "system__regexpS");
   u00152 : constant Version_32 := 16#fe1ffede#;
   pragma Export (C, u00152, "ada__strings__boundedB");
   u00153 : constant Version_32 := 16#89c18940#;
   pragma Export (C, u00153, "ada__strings__boundedS");
   u00154 : constant Version_32 := 16#7ec26662#;
   pragma Export (C, u00154, "ada__strings__superboundedB");
   u00155 : constant Version_32 := 16#da6addee#;
   pragma Export (C, u00155, "ada__strings__superboundedS");
   u00156 : constant Version_32 := 16#d5bfa9f3#;
   pragma Export (C, u00156, "ada__text_ioB");
   u00157 : constant Version_32 := 16#8d734ca7#;
   pragma Export (C, u00157, "ada__text_ioS");
   u00158 : constant Version_32 := 16#7a518daf#;
   pragma Export (C, u00158, "assemble_functionsB");
   u00159 : constant Version_32 := 16#aed3e8b3#;
   pragma Export (C, u00159, "assemble_functionsS");
   u00160 : constant Version_32 := 16#4b37b589#;
   pragma Export (C, u00160, "system__val_enumB");
   u00161 : constant Version_32 := 16#d83c821f#;
   pragma Export (C, u00161, "system__val_enumS");
   u00162 : constant Version_32 := 16#f6fdca1c#;
   pragma Export (C, u00162, "ada__text_io__integer_auxB");
   u00163 : constant Version_32 := 16#b9793d30#;
   pragma Export (C, u00163, "ada__text_io__integer_auxS");
   u00164 : constant Version_32 := 16#181dc502#;
   pragma Export (C, u00164, "ada__text_io__generic_auxB");
   u00165 : constant Version_32 := 16#a6c327d3#;
   pragma Export (C, u00165, "ada__text_io__generic_auxS");
   u00166 : constant Version_32 := 16#b10ba0c7#;
   pragma Export (C, u00166, "system__img_biuB");
   u00167 : constant Version_32 := 16#91823444#;
   pragma Export (C, u00167, "system__img_biuS");
   u00168 : constant Version_32 := 16#4e06ab0c#;
   pragma Export (C, u00168, "system__img_llbB");
   u00169 : constant Version_32 := 16#d04524ba#;
   pragma Export (C, u00169, "system__img_llbS");
   u00170 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00170, "system__img_lliB");
   u00171 : constant Version_32 := 16#7269955b#;
   pragma Export (C, u00171, "system__img_lliS");
   u00172 : constant Version_32 := 16#a756d097#;
   pragma Export (C, u00172, "system__img_llwB");
   u00173 : constant Version_32 := 16#7929072c#;
   pragma Export (C, u00173, "system__img_llwS");
   u00174 : constant Version_32 := 16#eb55dfbb#;
   pragma Export (C, u00174, "system__img_wiuB");
   u00175 : constant Version_32 := 16#ffc3b3d6#;
   pragma Export (C, u00175, "system__img_wiuS");
   u00176 : constant Version_32 := 16#13f22147#;
   pragma Export (C, u00176, "improved_trieB");
   u00177 : constant Version_32 := 16#ed6976f3#;
   pragma Export (C, u00177, "improved_trieS");
   u00178 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00178, "ada__containersS");
   u00179 : constant Version_32 := 16#bcec81df#;
   pragma Export (C, u00179, "ada__containers__helpersB");
   u00180 : constant Version_32 := 16#4adfc5eb#;
   pragma Export (C, u00180, "ada__containers__helpersS");
   u00181 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00181, "system__pool_globalB");
   u00182 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00182, "system__pool_globalS");
   u00183 : constant Version_32 := 16#a6359005#;
   pragma Export (C, u00183, "system__memoryB");
   u00184 : constant Version_32 := 16#3a5ba6be#;
   pragma Export (C, u00184, "system__memoryS");
   u00185 : constant Version_32 := 16#fd2ad2f1#;
   pragma Export (C, u00185, "gnatS");
   u00186 : constant Version_32 := 16#539386eb#;
   pragma Export (C, u00186, "gnat__command_lineB");
   u00187 : constant Version_32 := 16#687dd21e#;
   pragma Export (C, u00187, "gnat__command_lineS");
   u00188 : constant Version_32 := 16#cceeaf4e#;
   pragma Export (C, u00188, "gnat__directory_operationsB");
   u00189 : constant Version_32 := 16#e2bb2709#;
   pragma Export (C, u00189, "gnat__directory_operationsS");
   u00190 : constant Version_32 := 16#c024395a#;
   pragma Export (C, u00190, "gnat__os_libS");
   u00191 : constant Version_32 := 16#e51537a7#;
   pragma Export (C, u00191, "ada__command_lineB");
   u00192 : constant Version_32 := 16#d59e21a4#;
   pragma Export (C, u00192, "ada__command_lineS");
   u00193 : constant Version_32 := 16#084c16d0#;
   pragma Export (C, u00193, "gnat__regexpS");
   u00194 : constant Version_32 := 16#b4645806#;
   pragma Export (C, u00194, "gnat__stringsS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  ada.command_line%s
   --  gnat%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  gnat.strings%s
   --  system.os_lib%s
   --  gnat.os_lib%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.val_enum%s
   --  system.val_int%s
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  system.val_int%b
   --  system.val_enum%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.fixed%s
   --  ada.strings.maps.constants%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.superbounded%s
   --  ada.strings.bounded%s
   --  ada.strings.bounded%b
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  system.os_constants%s
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  gnat.directory_operations%s
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.file_attributes%s
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.object_reader%s
   --  system.dwarf_lines%s
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.secondary_stack%s
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  system.file_io%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.superbounded%b
   --  ada.strings.fixed%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  ada.command_line%b
   --  ada.characters.handling%b
   --  system.secondary_stack%b
   --  system.dwarf_lines%b
   --  system.object_reader%b
   --  gnat.directory_operations%b
   --  ada.calendar.formatting%b
   --  system.address_image%b
   --  ada.exceptions.traceback%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  ada.directories%s
   --  ada.directories.validity%s
   --  ada.directories.validity%b
   --  system.regexp%s
   --  system.regexp%b
   --  ada.directories%b
   --  gnat.regexp%s
   --  gnat.command_line%s
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  gnat.command_line%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  improved_trie%s
   --  improved_trie%b
   --  assemble_functions%s
   --  assemble_functions%b
   --  compiler%b
   --  END ELABORATION ORDER


end ada_main;
