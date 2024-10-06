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
        Array.of_list (List.map (fun _ -> i64_type) parameters)
      in
      let func_type = function_type llvm_return_type param_types in
      let the_function = define_function name func_type the_module in
      let entry = append_block context "entry" the_function in
      position_at_end entry builder;

      let return_generated = ref false in
      List.iter
        (fun stmt ->
          match stmt with
          | Stmt.ReturnStmt _ ->
              if not !return_generated then (
                let ret_val = codegen_stmt stmt in
                ignore (build_ret ret_val builder);
                return_generated := true)
          | _ when not !return_generated -> ignore (codegen_stmt stmt)
          | _ -> failwith "")
        body;

      if not !return_generated then
        ignore (build_ret (const_int i64_type 0) builder);

      the_function
  | Stmt.ReturnStmt expr -> codegen_expr expr
  | _ -> failwith "Statement not implemented"

let generate_code ast =
  ignore (codegen_stmt ast);
  print_module "output.ll" the_module
