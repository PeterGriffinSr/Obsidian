open Syntax

let print_error_position filename _lexbuf =
  let line_num = Lexer.get_line () in
  let col_num = Lexer.get_column () in

  let file_channel = open_in filename in
  let rec read_lines i =
    try
      let line = input_line file_channel in
      if i = line_num then Some line else read_lines (i + 1)
    with End_of_file -> None
  in
  let line_content = match read_lines 1 with Some line -> line | None -> "" in
  close_in file_channel;

  Printf.printf "Parser Error at Line %d, Column %d\n-> %s\n" line_num col_num
    line_content

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

(* let run_clang_commands () =
  let clang_s_command = "clang -S output.ll" in
  if Sys.command clang_s_command <> 0 then
    Printf.eprintf "Failed to run clang -S on output.ll\n";
  Sys.remove "output.ll";

  let clang_o_command = "clang -o output output.s -O3" in
  if Sys.command clang_o_command <> 0 then
    Printf.eprintf "Failed to run clang -o output on output.s\n";

  Sys.remove "output.s" *)

let () =
  if Array.length Sys.argv = 2 then (
    let filename = Sys.argv.(1) in
    let file_channel = open_in filename in
    let lexbuf = Lexing.from_channel file_channel in
    try
      let ast = Parser.program Lexer.token lexbuf in
      (* Printf.printf "Parsed AST:\n%s\n" (Ast.Stmt.show ast); *)

      let initial_env = Typechecker.TypeChecker.empty_env in

      let _ =
        Typechecker.TypeChecker.check_block initial_env [ ast ]
          ~expected_return_type:None
      in

      Codegen.generate_code ast;

      close_in file_channel;

      remove_entry1_lines "output.ll";
      
      (* run_clang_commands (); *)
    with
    | Parser.Error ->
        print_error_position filename lexbuf;
        Printf.fprintf stderr "Missing ';' at the end of the statement.\n";
        close_in file_channel;
        exit (-1)
    | Failure msg ->
        Printf.fprintf stderr "Error: %s\n" msg;
        close_in file_channel;
        exit (-1)
    | e ->
        Printf.fprintf stderr "An unexpected error occurred: %s\n"
          (Printexc.to_string e);
        close_in file_channel;
        exit (-1))
