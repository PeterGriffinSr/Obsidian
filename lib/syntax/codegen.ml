open Llvm
open Ast

let context = global_context ()
let the_module = create_module context "my_module"
let builder = builder context
let i32_type = i32_type context

let rec codegen_expr = function
  | Expr.IntExpr { value } -> const_int i32_type value
  | Expr.BinaryExpr { left; operator; right } -> (
      let left_val = codegen_expr left in
      let right_val = codegen_expr right in
      match operator with
      | Ast.Plus -> build_add left_val right_val "addtmp" builder
      | Ast.Minus -> build_sub left_val right_val "subtmp" builder
      | Ast.Star -> build_mul left_val right_val "multmp" builder
      | Ast.Slash -> build_sdiv left_val right_val "divtmp" builder
      | _ -> failwith "Operator not implemented")
  | _ -> failwith "Expression not implemented"

let rec codegen_stmt = function
  | Stmt.ExprStmt expr -> codegen_expr expr
  | Stmt.BlockStmt { body } ->
      List.fold_left
        (fun _ stmt -> codegen_stmt stmt)
        (const_int i32_type 0) body
  | _ -> failwith "Statement not implemented"

let create_main_function () =
  let main_type = function_type i32_type [||] in
  let main_func = define_function "main" main_type the_module in
  let entry = append_block context "entry" main_func in
  position_at_end entry builder;
  main_func

let generate_code ast =
  let main_func = create_main_function () in
  ignore main_func;
  let result = codegen_stmt ast in
  ignore (build_ret result builder);
  print_module "output.ll" the_module
