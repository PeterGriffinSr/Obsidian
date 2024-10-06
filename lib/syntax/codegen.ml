open Llvm
open Ast

let context = global_context ()
let the_module = create_module context "obsidian"
let builder = builder context
let i64_type = i64_type context
let f64_type = double_type context
let i1_type = i1_type context
let char_type = i8_type context
let void_type = void_type context
let string_type = pointer_type (i8_type context)
let variables = Hashtbl.create 10
let printf_type = var_arg_function_type i64_type [| string_type |]
let printf_func = declare_function "printf" printf_type the_module

let print_any_type value llvm_type =
  match classify_type llvm_type with
  | TypeKind.Integer ->
      let format_str = build_global_stringptr "%ld\n" "int_fmt" builder in
      build_call printf_func [| format_str; value |] "printtmp" builder
  | TypeKind.Double ->
      let format_str = build_global_stringptr "%f\n" "float_fmt" builder in
      build_call printf_func [| format_str; value |] "printtmp" builder
  | TypeKind.Pointer ->
      let format_str = build_global_stringptr "%s\n" "str_fmt" builder in
      build_call printf_func [| format_str; value |] "printtmp" builder
  | TypeKind.Void -> failwith "Cannot print void type"
  | _ -> failwith "Unsupported type for println"

let rec codegen_expr = function
  | Expr.IntExpr { value } -> const_int i64_type value
  | Expr.FloatExpr { value } -> const_float f64_type value
  | Expr.CharExpr { value } -> const_int char_type (Char.code value)
  | Expr.StringExpr { value } -> build_global_stringptr value "strtmp" builder
  | Expr.BoolExpr { value } -> const_int i1_type (if value then 1 else 0)
  | Expr.BinaryExpr { left; operator; right } -> (
      let left_val = codegen_expr left in
      let right_val = codegen_expr right in
      let left_is_float = classify_type (type_of left_val) = TypeKind.Double in
      let right_is_float =
        classify_type (type_of right_val) = TypeKind.Double
      in
      match (operator, left_is_float, right_is_float) with
      | Ast.Plus, true, true -> build_fadd left_val right_val "faddtmp" builder
      | Ast.Minus, true, true -> build_fsub left_val right_val "fsubtmp" builder
      | Ast.Star, true, true -> build_fmul left_val right_val "fmultmp" builder
      | Ast.Slash, true, true -> build_fdiv left_val right_val "fdivtmp" builder
      | Ast.Plus, false, false -> build_add left_val right_val "addtmp" builder
      | Ast.Minus, false, false -> build_sub left_val right_val "subtmp" builder
      | Ast.Star, false, false -> build_mul left_val right_val "multmp" builder
      | Ast.Slash, false, false ->
          build_sdiv left_val right_val "divtmp" builder
      | _ -> failwith "Mixed or unsupported operand types for binary operation")
  | Expr.VarExpr identifier -> (
      try
        let var_alloca = Hashtbl.find variables identifier in
        build_load var_alloca identifier builder
      with Not_found -> failwith ("Unknown variable: " ^ identifier))
  | Expr.CallExpr { callee; arguments } -> (
      let func_name =
        match callee with
        | Expr.VarExpr name -> name
        | _ -> failwith "Codegen error: Unsupported function call format"
      in
      match lookup_function func_name the_module with
      | Some func ->
          let arg_vals = List.map codegen_expr arguments in
          let arg_vals_array = Array.of_list arg_vals in
          if Array.length arg_vals_array != Array.length (params func) then
            failwith
              ("Codegen error: Incorrect number of arguments for function "
             ^ func_name);
          build_call func arg_vals_array "calltmp" builder
      | None -> failwith ("Codegen error: Unknown function " ^ func_name))
  | Expr.PrintlnExpr { expr } ->
      let value = codegen_expr expr in
      let llvm_type = type_of value in
      print_any_type value llvm_type
  | _ -> failwith "Expression not implemented"

let rec codegen_stmt = function
  | Stmt.ExprStmt expr -> codegen_expr expr
  | Stmt.VarDeclarationStmt
      { identifier; constant = _; assigned_value; explicit_type } ->
      let llvm_type =
        match explicit_type with
        | Ast.Type.SymbolType { value = "int" } -> i64_type
        | Ast.Type.SymbolType { value = "float" } -> f64_type
        | Ast.Type.SymbolType { value = "char" } -> char_type
        | Ast.Type.SymbolType { value = "string" } -> string_type
        | Ast.Type.SymbolType { value = "bool" } -> i1_type
        | _ -> failwith "Unsupported type for variable declaration"
      in
      let alloca = build_alloca llvm_type identifier builder in
      Hashtbl.add variables identifier alloca;
      (match assigned_value with
      | Some expr ->
          let initial_value = codegen_expr expr in
          ignore (build_store initial_value alloca builder)
      | None -> ());
      alloca
  | Stmt.BlockStmt { body } ->
      List.fold_left
        (fun _ stmt -> codegen_stmt stmt)
        (const_int i64_type 0) body
  | Stmt.FuncDeclStmt { name; parameters; return_type; body } ->
      let llvm_return_type =
        match return_type with
        | Some (Ast.Type.SymbolType { value = "int" }) -> i64_type
        | Some (Ast.Type.SymbolType { value = "float" }) -> f64_type
        | Some (Ast.Type.SymbolType { value = "char" }) -> char_type
        | Some (Ast.Type.SymbolType { value = "string" }) -> string_type
        | Some (Ast.Type.SymbolType { value = "bool" }) -> i1_type
        | Some (Ast.Type.SymbolType { value = "void" }) -> void_type
        | _ -> failwith "Unsupported function return type"
      in
      let param_types =
        Array.of_list
          (List.map
             (fun param ->
               match param.Stmt.param_type with
               | Ast.Type.SymbolType { value = "int" } -> i64_type
               | Ast.Type.SymbolType { value = "float" } -> f64_type
               | Ast.Type.SymbolType { value = "char" } -> char_type
               | Ast.Type.SymbolType { value = "string" } -> string_type
               | Ast.Type.SymbolType { value = "bool" } -> i1_type
               | _ -> failwith "Unsupported parameter type")
             parameters)
      in
      let func_type = function_type llvm_return_type param_types in
      let the_function = define_function name func_type the_module in
      let entry = append_block context "entry" the_function in
      position_at_end entry builder;

      Array.iteri
        (fun i param_val ->
          let param_name = (List.nth parameters i).Stmt.name in
          set_value_name param_name param_val;
          let alloca = build_alloca (type_of param_val) param_name builder in
          ignore (build_store param_val alloca builder);
          Hashtbl.add variables param_name alloca)
        (params the_function);

      let return_generated = ref false in
      List.iter
        (fun stmt ->
          match stmt with
          | Stmt.ReturnStmt _ when not !return_generated ->
              let ret_val = codegen_stmt stmt in
              ignore (build_ret ret_val builder);
              return_generated := true
          | _ when not !return_generated -> ignore (codegen_stmt stmt)
          | _ -> ())
        body;

      the_function
  | Stmt.ReturnStmt expr -> codegen_expr expr
  | _ -> failwith "Statement not implemented"

let generate_code ast =
  ignore (codegen_stmt ast);
  print_module "output.ll" the_module
