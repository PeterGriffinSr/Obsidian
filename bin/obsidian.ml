open Obsidianlib
open Typechecker

let print_help () =
  Printf.printf "Usage: obsidian [options] file...\n";
  Printf.printf "Options:\n";
  Printf.printf "  --help           Display this information.\n";
  Printf.printf "  --help={optimizers|warnings|target}[,...].\n\n";
  Printf.printf "  --version        Display compiler version information.\n\n";
  Printf.printf "  -save-temps      Do not delete intermediate files.\n\n";
  Printf.printf "  -S               Compile only; do not assemble or link.\n";
  Printf.printf "  -c               Compile and assemble, but do not link.\n";
  Printf.printf "  -o <file>        Place the output into <file>.\n"

let print_version () =
  Printf.printf "Obsidian Compiler Version: %s\n" (Version.version ())

let remove_entry1_lines filename =
  let in_channel = open_in filename in
  let out_channel = open_out "filtered_a.out.ll" in
  try
    while true do
      let line = input_line in_channel in
      if not (Str.string_match (Str.regexp ".*entry1:") line 0) then
        output_string out_channel (line ^ "\n")
    done
  with End_of_file ->
    close_in in_channel;
    close_out out_channel;
    Sys.rename "filtered_a.out.ll" filename

let print_error_position filename saved_line saved_column =
  let file_channel = open_in filename in
  let rec read_lines i =
    try
      let line = input_line file_channel in
      if i = saved_line then Some line else read_lines (i + 1)
    with End_of_file -> None
  in
  let line_content = match read_lines 1 with Some line -> line | None -> "" in
  close_in file_channel;
  Printf.printf "Error at Line %d, Column %d\n" saved_line saved_column;
  Printf.printf "%s\n" line_content;
  Printf.printf "%s^\n" (String.make saved_column ' ')

let run_clang_commands ~output ~save_asm ~compile_only ~link_math ~debug ~wall
    ~custom_flags ~optimization_level ~save_temps =
  if save_asm || save_temps then (
    let clang_s_command =
      Printf.sprintf "clang -S a.out.ll %s -Wno-override-module -o %s.s"
        custom_flags output
    in
    if Sys.command clang_s_command <> 0 then
      Printf.eprintf "Failed to run clang -S on a.out.ll\n";
    if not save_temps then Sys.remove "a.out.ll");

  if compile_only || save_temps then (
    let clang_c_command =
      Printf.sprintf "clang -c a.out.ll %s -Wno-override-module -o %s.o"
        custom_flags output
    in
    if Sys.command clang_c_command <> 0 then
      Printf.eprintf "Failed to run clang -c on a.out.ll\n";
    if not save_temps then Sys.remove "a.out.ll");

  let math_lib_flag = if link_math then "-lm" else "" in
  let debug_flag = if debug then "-g" else "" in
  let wall_flag = if wall then "-Wall" else "" in
  let opt_flag =
    match optimization_level with
    | "" -> ""
    | level -> Printf.sprintf "-O%s" level
  in
  let clang_o_command =
    Printf.sprintf "clang -o %s a.out.ll %s %s %s %s %s -Wno-override-module"
      output opt_flag debug_flag wall_flag math_lib_flag custom_flags
  in
  if Sys.command clang_o_command <> 0 then
    Printf.eprintf "Failed to run clang -o on a.out.ll\n";

  if not save_temps then Sys.remove "a.out.ll"

let () =
  let output_name = ref "a.out" in
  let save_asm = ref false in
  let save_temps = ref false in
  let compile_only = ref false in
  let link_math = ref false in
  let debug = ref false in
  let wall = ref false in
  let optimization_level = ref "" in
  let custom_flags = ref "" in
  let file_name = ref "" in

  let speclist =
    [
      ( "--help",
        Arg.Unit
          (fun () ->
            print_help ();
            exit 0),
        "Display this help message" );
      ( "--version",
        Arg.Unit
          (fun () ->
            print_version ();
            exit 0),
        "Display compiler version information" );
      ("-S", Arg.Set save_asm, "Compile only; do not assemble or link.");
      ("-c", Arg.Set compile_only, "Compile and assemble, but do not link.");
      ("-save-temps", Arg.Set save_temps, "Do not delete intermediate files.");
      ("-lm", Arg.Set link_math, "Link the math library.");
      ("-g", Arg.Set debug, "Generate debug information.");
      ("-Wall", Arg.Set wall, "Enable all compiler warnings.");
      ( "-o",
        Arg.String (fun s -> output_name := s),
        "Specify the output file name" );
      ( "-O1",
        Arg.Unit (fun () -> optimization_level := "1"),
        "Optimization level 1" );
      ( "-O2",
        Arg.Unit (fun () -> optimization_level := "2"),
        "Optimization level 2" );
      ( "-O3",
        Arg.Unit (fun () -> optimization_level := "3"),
        "Optimization level 3" );
    ]
  in

  Arg.parse speclist (fun s -> file_name := s) "Options:";

  if !file_name = "" then (
    Printf.eprintf "obsidian: error: no input files\n";
    exit 1);

  let filename = !file_name in
  let file_channel = open_in filename in
  let lexbuf = Lexing.from_channel file_channel in

  try
    let ast = Parser.program Lexer.token lexbuf in
    Printf.printf "Parsed AST: %s\n" (Ast.Stmt.show ast);

    let initial_env = TypeChecker.empty_env in

    let _ =
      try
        TypeChecker.check_block initial_env [ ast ] ~expected_return_type:None
      with
      | TypeError msg ->
          Printf.eprintf "%s\n" msg;
          close_in file_channel;
          exit (-1)
      | UnboundFunctionError msg ->
          Printf.eprintf "%s\n" msg;
          close_in file_channel;
          exit (-1)
      | UnboundVariableError msg ->
          Printf.eprintf "%s\n" msg;
          close_in file_channel;
          exit (-1)
      | UnsupportedOperationError msg ->
          Printf.eprintf "%s\n" msg;
          close_in file_channel;
          exit (-1)
      | ArgumentMismatchError msg ->
          Printf.eprintf "%s\n" msg;
          close_in file_channel;
          exit (-1)
      | ReturnTypeError msg ->
          Printf.eprintf "%s\n" msg;
          close_in file_channel;
          exit (-1)
    in

    Codegen.generate_code ast;
    close_in file_channel;

    remove_entry1_lines "a.out.ll";
    run_clang_commands ~output:!output_name ~save_asm:!save_asm
      ~compile_only:!compile_only ~link_math:!link_math ~debug:!debug
      ~wall:!wall ~custom_flags:!custom_flags
      ~optimization_level:!optimization_level ~save_temps:!save_temps
  with
  | Parser.Error ->
      print_error_position filename (Lexer.get_line ()) (Lexer.get_column ());
      Printf.fprintf stderr
        "Syntax Error: Check for missing or misplaced ';'.\n";
      close_in file_channel;
      exit (-1)
  | Failure msg ->
      Printf.fprintf stderr "Error: %s\n" msg;
      close_in file_channel;
      exit (-1)
  | e ->
      Printf.fprintf stderr "Unhandled Exception: %s\n" (Printexc.to_string e);
      close_in file_channel;
      exit (-1)
