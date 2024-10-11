open Syntax
open Typechecker

let print_help () =
  Printf.printf "Usage: obsidian [options] file...\n";
  Printf.printf "Options:\n";
  Printf.printf "  --help           Display this information.\n";
  Printf.printf "  -S               Compile only; do not assemble or link.\n";
  Printf.printf "  -o <file>        Place the output into <file>.\n"

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

let remove_entry1_lines filename =
  let in_channel = open_in filename in
  let out_channel = open_out "filtered_output.ll" in
  try
    while true do
      let line = input_line in_channel in
      if not (Str.string_match (Str.regexp ".*entry1:") line 0) then
        output_string out_channel (line ^ "\n")
    done
  with End_of_file ->
    close_in in_channel;
    close_out out_channel;
    Sys.rename "filtered_output.ll" filename

let run_clang_commands ~output ~save_asm ~optimization_level =
  if save_asm then (
    let clang_s_command =
      Printf.sprintf "clang -S output.ll -O%s -Wno-override-module"
        optimization_level
    in
    if Sys.command clang_s_command <> 0 then
      Printf.eprintf "Failed to run clang -S on output.ll\n";
    Sys.remove "output.ll";
    exit 0);

  let clang_o_command =
    Printf.sprintf "clang -o %s output.ll -O%s -Wno-override-module -lm" output
      optimization_level
  in
  if Sys.command clang_o_command <> 0 then
    Printf.eprintf "Failed to run clang -o on output.ll\n";

  Sys.remove "output.ll"

let () =
  let output_name = ref "a.out" in
  let save_asm = ref false in
  let optimization_level = ref "" in
  let file_name = ref "" in

  let speclist =
    [
      ( "--help",
        Arg.Unit
          (fun () ->
            print_help ();
            exit 0),
        "Display this help message" );
      ("-S", Arg.Set save_asm, "Compile only; do not assemble or link.");
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
        "Optimization level 3 (default)" );
    ]
  in

  Arg.parse speclist (fun s -> file_name := s) "Options:";

  if !file_name = "" then (
    Printf.eprintf "Error: No input file provided\n";
    print_help ();
    exit 1);

  let filename = !file_name in
  let file_channel = open_in filename in
  let lexbuf = Lexing.from_channel file_channel in

  try
    let ast = Parser.program Lexer.token lexbuf in

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

    remove_entry1_lines "output.ll";
    run_clang_commands ~output:!output_name ~save_asm:!save_asm
      ~optimization_level:!optimization_level
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
