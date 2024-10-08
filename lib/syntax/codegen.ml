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
let malloc_type = function_type string_type [| i64_type |]
let malloc_func = declare_function "malloc" malloc_type the_module
let scanf_type = var_arg_function_type i64_type [| string_type |]
let scanf_func = declare_function "scanf" scanf_type the_module

let free_func =
  declare_function "free" (function_type void_type [| string_type |]) the_module

let strcpy_func =
  declare_function "strcpy"
    (function_type string_type [| string_type; string_type |])
    the_module

let strcat_func =
  declare_function "strcat"
    (function_type string_type [| string_type; string_type |])
    the_module

let strlen_func =
  declare_function "strlen"
    (function_type i64_type [| string_type |])
    the_module

let concatenate_strings left right =
  let left_len = build_call strlen_func [| left |] "leftlen" builder in
  let right_len = build_call strlen_func [| right |] "rightlen" builder in

  let total_len = build_add left_len right_len "totallen" builder in
  let total_len_with_null =
    build_add total_len (const_int i64_type 1) "lenwithnull" builder
  in

  let result =
    build_call malloc_func [| total_len_with_null |] "concatresult" builder
  in

  ignore (build_call strcpy_func [| result; left |] "cpyres" builder);

  ignore (build_call strcat_func [| result; right |] "catres" builder);

  result

let type_size_in_bytes = function
  | Ast.Type.SymbolType { value = "int" } -> 8
  | Ast.Type.SymbolType { value = "float" } -> 8
  | Ast.Type.SymbolType { value = "char" } -> 1
  | Ast.Type.SymbolType { value = "string" } -> 8
  | Ast.Type.SymbolType { value = "bool" } -> 1
  | _ -> failwith "Unsupported type for sizeof"

let string_of_llvm_type llvm_type =
  match classify_type llvm_type with
  | TypeKind.Integer -> "int"
  | TypeKind.Double -> "float"
  | TypeKind.Pointer -> "string"
  | TypeKind.Void -> "void"
  | TypeKind.Float -> "float"
  | TypeKind.Function -> "function"
  | _ -> "unknown"

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
      | Ast.Eq, false, false ->
          build_icmp Icmp.Eq left_val right_val "eqtmp" builder
      | Ast.Neq, false, false ->
          build_icmp Icmp.Ne left_val right_val "neqtmp" builder
      | Ast.Leq, false, false ->
          build_icmp Icmp.Sle left_val right_val "leqtmp" builder
      | Ast.Geq, false, false ->
          build_icmp Icmp.Sge left_val right_val "geqtmp" builder
      | Ast.Less, false, false ->
          build_icmp Icmp.Slt left_val right_val "letmp" builder
      | Ast.Greater, false, false ->
          build_icmp Icmp.Sgt left_val right_val "getmp" builder
      | Ast.LogicalAnd, false, false ->
          build_and left_val right_val "andtmp" builder
      | Ast.LogicalOr, false, false ->
          build_or left_val right_val "ortmp" builder
      | Ast.Carot, false, false -> concatenate_strings left_val right_val
      | _ -> failwith "Mixed or unsupported operand types for binary operation")
  | Expr.SizeofExpr { type_expr } ->
      let size_in_bytes = type_size_in_bytes type_expr in
      const_int i64_type size_in_bytes
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
          let func_type = return_type (element_type (type_of func)) in
          if Array.length arg_vals_array != Array.length (params func) then
            failwith
              ("Codegen error: Incorrect number of arguments for function "
             ^ func_name);
          if classify_type func_type = TypeKind.Void then
            build_call func arg_vals_array "" builder
          else build_call func arg_vals_array "calltmp" builder
      | None -> failwith ("Codegen error: Unknown function " ^ func_name))
  | Expr.PrintlnExpr { expr } -> (
      let value = codegen_expr expr in
      let llvm_type = type_of value in
      match classify_type llvm_type with
      | TypeKind.Pointer ->
          let format_str = build_global_stringptr "%s\n" "str_fmt" builder in
          ignore
            (build_call printf_func [| format_str; value |] "printtmp" builder);
          let _ = build_call free_func [| value |] "" builder in
          value
      | _ -> print_any_type value llvm_type)
  | Expr.UnaryExpr { operator; operand } -> (
      let var_val = codegen_expr operand in
      let var_name =
        match operand with
        | Expr.VarExpr identifier -> identifier
        | _ -> failwith "Increment/Decrement can only be applied to variables"
      in
      match operator with
      | Ast.Not -> build_not var_val "nottmp" builder
      | Ast.Inc ->
          let incremented =
            build_add var_val (const_int i64_type 1) "inctmp" builder
          in
          let var_alloca = Hashtbl.find variables var_name in
          ignore (build_store incremented var_alloca builder);
          incremented
      | Ast.Dec ->
          let decremented =
            build_sub var_val (const_int i64_type 1) "dectmp" builder
          in
          let var_alloca = Hashtbl.find variables var_name in
          ignore (build_store decremented var_alloca builder);
          decremented
      | _ -> failwith "Unsupported unary operator")
  | Expr.CastExpr { expr; target_type } -> (
      let value = codegen_expr expr in
      let target_llvm_type =
        match target_type with
        | Ast.Type.SymbolType { value = "int" } -> i64_type
        | Ast.Type.SymbolType { value = "float" } -> f64_type
        | Ast.Type.SymbolType { value = "string" } -> string_type
        | _ -> failwith "Unsupported target type for cast"
      in
      match (classify_type (type_of value), classify_type target_llvm_type) with
      | TypeKind.Integer, TypeKind.Double ->
          build_sitofp value target_llvm_type "casttmp" builder
      | TypeKind.Double, TypeKind.Integer ->
          build_fptosi value target_llvm_type "casttmp" builder
      | TypeKind.Pointer, TypeKind.Pointer ->
          build_bitcast value target_llvm_type "casttmp" builder
      | TypeKind.Integer, TypeKind.Pointer ->
          build_inttoptr value target_llvm_type "casttmp" builder
      | TypeKind.Pointer, TypeKind.Integer ->
          build_ptrtoint value target_llvm_type "casttmp" builder
      | _, _ when type_of value = target_llvm_type -> value
      | _ -> failwith "Unsupported cast operation")
  | Expr.TypeofExpr { expr } ->
      let value = codegen_expr expr in
      let llvm_type_str = string_of_llvm_type (type_of value) in
      build_global_stringptr llvm_type_str "typestrtmp" builder
  | Expr.LengthExpr { expr } ->
      let value = codegen_expr expr in
      if classify_type (type_of value) <> TypeKind.Pointer then
        failwith "LengthExpr is only valid for strings";
      let strlen_function =
        match lookup_function "strlen" the_module with
        | Some f -> f
        | None ->
            let strlen_type = function_type i64_type [| string_type |] in
            declare_function "strlen" strlen_type the_module
      in
      build_call strlen_function [| value |] "lentmp" builder
  | Expr.InputExpr { prompt; target_type } -> (
      let prompt_str = build_global_stringptr prompt "promptstr" builder in
      ignore (build_call printf_func [| prompt_str |] "printprompt" builder);

      match target_type with
      | Ast.Type.SymbolType { value = "int" } ->
          let int_ptr = build_alloca i64_type "intinput" builder in
          let format_str = build_global_stringptr "%ld" "int_fmt" builder in
          ignore (build_call scanf_func [| format_str; int_ptr |] "" builder);
          build_load int_ptr "intresult" builder
      | Ast.Type.SymbolType { value = "float" } ->
          let float_ptr = build_alloca f64_type "floatinput" builder in
          let format_str = build_global_stringptr "%lf" "float_fmt" builder in
          ignore (build_call scanf_func [| format_str; float_ptr |] "" builder);
          build_load float_ptr "floatresult" builder
      | Ast.Type.SymbolType { value = "char" } ->
          let char_ptr = build_alloca char_type "charinput" builder in
          let format_str = build_global_stringptr "%c" "char_fmt" builder in
          ignore (build_call scanf_func [| format_str; char_ptr |] "" builder);
          build_load char_ptr "charresult" builder
      | Ast.Type.SymbolType { value = "string" } ->
          let str_ptr =
            build_call malloc_func
              [| const_int i64_type 256 |]
              "strinput" builder
          in
          let format_str = build_global_stringptr "%s" "str_fmt" builder in
          ignore (build_call scanf_func [| format_str; str_ptr |] "" builder);
          str_ptr
      | _ -> failwith "Codegen: Unsupported type for input")
  | Expr.AssignmentExpr { identifier; value = Some expr_value } -> (
      try
        let var_alloca = Hashtbl.find variables identifier in
        let new_value = codegen_expr expr_value in
        ignore (build_store new_value var_alloca builder);
        new_value
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
      let is_void_return = llvm_return_type = void_type in

      List.iter
        (fun stmt ->
          match stmt with
          | Stmt.ReturnStmt _ when not !return_generated ->
              if is_void_return then (
                ignore (build_ret_void builder);
                return_generated := true)
              else
                let ret_val = codegen_stmt stmt in
                ignore (build_ret ret_val builder);
                return_generated := true
          | _ when not !return_generated -> ignore (codegen_stmt stmt)
          | _ -> ())
        body;

      if (not !return_generated) && is_void_return then
        ignore (build_ret_void builder);

      the_function
  | Stmt.ReturnStmt expr -> codegen_expr expr
  | Stmt.SwitchStmt { expr; cases; default_case } ->
      let cond_val = codegen_expr expr in
      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in
      let merge_bb = append_block context "switchcont" the_function in
      let default_bb =
        match default_case with
        | Some _ -> append_block context "default" the_function
        | None -> merge_bb
      in

      let switch_inst =
        build_switch cond_val default_bb (List.length cases) builder
      in

      List.iter
        (fun (case_val, case_body) ->
          let case_block = append_block context "case" the_function in
          position_at_end case_block builder;

          List.iter (fun stmt -> ignore (codegen_stmt stmt)) case_body;

          ignore (build_br merge_bb builder);

          let case_const = codegen_expr case_val in
          add_case switch_inst case_const case_block)
        cases;

      (match default_case with
      | Some default_body ->
          position_at_end default_bb builder;
          List.iter (fun stmt -> ignore (codegen_stmt stmt)) default_body;
          ignore (build_br merge_bb builder)
      | None -> ());

      position_at_end merge_bb builder;
      const_int i64_type 0
  | Stmt.IfStmt { condition; then_branch; else_branch } ->
      let cond_val = codegen_expr condition in
      let zero = const_int i1_type 0 in
      let cond_bool = build_icmp Icmp.Ne cond_val zero "ifcond" builder in
      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in
      let then_bb = append_block context "then" the_function in
      let else_bb = append_block context "else" the_function in
      let merge_bb = append_block context "ifcont" the_function in
      ignore (build_cond_br cond_bool then_bb else_bb builder);
      position_at_end then_bb builder;
      ignore (codegen_stmt then_branch);
      ignore (build_br merge_bb builder);
      position_at_end else_bb builder;
      (match else_branch with
      | Some body -> ignore (codegen_stmt body)
      | None -> ());
      ignore (build_br merge_bb builder);
      position_at_end merge_bb builder;
      const_int i64_type 0
  | Stmt.WhileStmt { expr; body } ->
      let cond_bb =
        append_block context "while.cond"
          (block_parent (insertion_block builder))
      in
      let body_bb =
        append_block context "while.body"
          (block_parent (insertion_block builder))
      in
      let after_bb =
        append_block context "while.end"
          (block_parent (insertion_block builder))
      in

      ignore (build_br cond_bb builder);
      position_at_end cond_bb builder;

      let cond_val = codegen_expr expr in
      let zero = const_int i1_type 0 in
      let cond_bool = build_icmp Icmp.Ne cond_val zero "whilecond" builder in
      ignore (build_cond_br cond_bool body_bb after_bb builder);

      position_at_end body_bb builder;
      List.iter (fun stmt -> ignore (codegen_stmt stmt)) body;
      ignore (build_br cond_bb builder);

      position_at_end after_bb builder;
      const_int i64_type 0
  | Stmt.ForStmt { initialization; condition; iteration; body } ->
      let init_bb = insertion_block builder in
      let cond_bb = append_block context "for.cond" (block_parent init_bb) in
      let body_bb = append_block context "for.body" (block_parent init_bb) in
      let iter_bb = append_block context "for.iter" (block_parent init_bb) in
      let after_bb = append_block context "for.end" (block_parent init_bb) in

      (match initialization with
      | Some init_stmt -> ignore (codegen_stmt init_stmt)
      | None -> ());

      ignore (build_br cond_bb builder);
      position_at_end cond_bb builder;

      let cond_val = codegen_expr condition in
      let zero = const_int i1_type 0 in
      let cond_bool = build_icmp Icmp.Ne cond_val zero "forcond" builder in
      ignore (build_cond_br cond_bool body_bb after_bb builder);

      position_at_end body_bb builder;
      ignore (codegen_stmt body);
      ignore (build_br iter_bb builder);

      position_at_end iter_bb builder;
      (match iteration with
      | Some iter_stmt -> ignore (codegen_stmt iter_stmt)
      | None -> ());
      ignore (build_br cond_bb builder);

      position_at_end after_bb builder;
      const_int i64_type 0
  | _ -> failwith "Statement not implemented"

let generate_code ast =
  ignore (codegen_stmt ast);
  print_module "output.ll" the_module
