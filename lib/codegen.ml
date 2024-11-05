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
let struct_type = Hashtbl.create 10
let enum_values = Hashtbl.create 10
let printf_type = var_arg_function_type i64_type [| string_type |]
let printf_func = declare_function "printf" printf_type the_module
let malloc_type = function_type string_type [| i64_type |]
let malloc_func = declare_function "malloc" malloc_type the_module
let scanf_type = var_arg_function_type i64_type [| string_type |]
let scanf_func = declare_function "scanf" scanf_type the_module

let find_field_index fields field_name =
  let rec aux index = function
    | [] -> raise (Failure ("Field " ^ field_name ^ " not found"))
    | (f, _) :: _ when f = field_name -> index
    | _ :: rest -> aux (index + 1) rest
  in
  aux 0 fields

let get_struct_type name =
  try
    let struct_type, fields = Hashtbl.find struct_type name in
    (struct_type, fields)
  with Not_found -> failwith ("Unknown struct type: " ^ name)

let define_struct_type name (fields : (string * Type.t) list) =
  let field_types =
    Array.of_list
      (List.map
         (fun (_, t) ->
           match t with
           | Type.SymbolType { value = "int" } -> i64_type
           | Type.SymbolType { value = "float" } -> f64_type
           | Type.SymbolType { value = "string" } -> string_type
           | Type.SymbolType { value = "bool" } -> i1_type
           | _ -> failwith ("Unsupported field type in struct: " ^ name))
         fields)
  in
  let llvm_struct = named_struct_type context name in
  struct_set_body llvm_struct field_types false;
  Hashtbl.add struct_type name (llvm_struct, fields);
  llvm_struct

let get_struct_instance struct_name =
  try Hashtbl.find variables struct_name
  with Not_found -> failwith ("Unknown struct instance: " ^ struct_name)

let get_struct_instance_opt struct_name =
  try Some (Hashtbl.find variables struct_name) with Not_found -> None

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

let declare_malloc_function _context the_module =
  let i8_ptr_type = pointer_type i64_type in
  let malloc_type = function_type i8_ptr_type [| i64_type |] in
  declare_function "malloc" malloc_type the_module

let type_size_in_bytes = function
  | Ast.Type.SymbolType { value = "int" } -> 8
  | Ast.Type.SymbolType { value = "float" } -> 8
  | Ast.Type.SymbolType { value = "char" } -> 1
  | Ast.Type.SymbolType { value = "string" } -> 8
  | Ast.Type.SymbolType { value = "bool" } -> 1
  | _ -> failwith "Codegen: Unsupported type for sizeof"

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
      let value_type = integer_bitwidth llvm_type in
      if value_type = 8 then (
        let format_str = build_global_stringptr "%c\n\\0" "char_fmt" builder in
        ignore
          (build_call printf_func [| format_str; value |] "printtmp" builder);
        value)
      else
        let format_str = build_global_stringptr "%ld\n" "int_fmt" builder in
        ignore
          (build_call printf_func [| format_str; value |] "printtmp" builder);
        value
  | TypeKind.Double ->
      let format_str = build_global_stringptr "%f\n" "float_fmt" builder in
      ignore (build_call printf_func [| format_str; value |] "printtmp" builder);
      value
  | TypeKind.Pointer ->
      let format_str = build_global_stringptr "%s\n\\0" "str_fmt" builder in
      ignore (build_call printf_func [| format_str; value |] "printtmp" builder);
      value
  | TypeKind.Void -> failwith "Codegen: Cannot print void type"
  | _ -> failwith "Codegen: Unsupported type for println"

let is_char_type llvm_type =
  match classify_type llvm_type with
  | TypeKind.Integer -> integer_bitwidth llvm_type = 8
  | _ -> false

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
      | Ast.PlusAssign, false, false ->
          let var_name =
            match left with
            | Expr.VarExpr identifier -> identifier
            | _ -> failwith "Codegen: Left-hand side of += must be a variable"
          in
          let var_alloca = Hashtbl.find variables var_name in
          let updated_value = build_add left_val right_val "addtmp" builder in
          ignore (build_store updated_value var_alloca builder);
          updated_value
      | Ast.MinusAssign, false, false ->
          let var_name =
            match left with
            | Expr.VarExpr identifier -> identifier
            | _ -> failwith "Codegen: Left-hand side of -= must be a variable"
          in
          let var_alloca = Hashtbl.find variables var_name in
          let updated_value = build_sub left_val right_val "subtmp" builder in
          ignore (build_store updated_value var_alloca builder);
          updated_value
      | Ast.StarAssign, false, false ->
          let var_name =
            match left with
            | Expr.VarExpr identifier -> identifier
            | _ -> failwith "Codegen: Left-hand side of *= must be a variable"
          in
          let var_alloca = Hashtbl.find variables var_name in
          let updated_value = build_mul left_val right_val "multmp" builder in
          ignore (build_store updated_value var_alloca builder);
          updated_value
      | Ast.SlashAssign, false, false ->
          let var_name =
            match left with
            | Expr.VarExpr identifier -> identifier
            | _ -> failwith "Codegen: Left-hand side of /= must be a variable"
          in
          let var_alloca = Hashtbl.find variables var_name in
          let updated_value = build_sdiv left_val right_val "divtmp" builder in
          ignore (build_store updated_value var_alloca builder);
          updated_value
      | Ast.Percent, false, false ->
          build_srem left_val right_val "modtmp" builder
      | Ast.Pipe, false, false -> build_or left_val right_val "bortmp" builder
      | Ast.Leftshift, false, false ->
          build_shl left_val right_val "shltmp" builder
      | Ast.Rightshift, false, false ->
          build_lshr left_val right_val "lshrtmp" builder
      | Ast.Xor, false, false -> build_xor left_val right_val "xortmp" builder
      | Ast.Ampersand, false, false ->
          build_and left_val right_val "bandtmp" builder
      | Ast.PlusAssign, true, true ->
          let var_name =
            match left with
            | Expr.VarExpr identifier -> identifier
            | _ -> failwith "Codegen: Left-hand side of += must be a variable"
          in
          let var_alloca = Hashtbl.find variables var_name in
          let updated_value = build_fadd left_val right_val "faddtmp" builder in
          ignore (build_store updated_value var_alloca builder);
          updated_value
      | Ast.MinusAssign, true, true ->
          let var_name =
            match left with
            | Expr.VarExpr identifier -> identifier
            | _ -> failwith "Codegen: Left-hand side of -= must be a variable"
          in
          let var_alloca = Hashtbl.find variables var_name in
          let updated_value = build_fsub left_val right_val "fsubtmp" builder in
          ignore (build_store updated_value var_alloca builder);
          updated_value
      | Ast.StarAssign, true, true ->
          let var_name =
            match left with
            | Expr.VarExpr identifier -> identifier
            | _ -> failwith "Codegen: Left-hand side of *= must be a variable"
          in
          let var_alloca = Hashtbl.find variables var_name in
          let updated_value = build_fmul left_val right_val "fmultmp" builder in
          ignore (build_store updated_value var_alloca builder);
          updated_value
      | Ast.SlashAssign, true, true ->
          let var_name =
            match left with
            | Expr.VarExpr identifier -> identifier
            | _ -> failwith "Codegen: Left-hand side of /= must be a variable"
          in
          let var_alloca = Hashtbl.find variables var_name in
          let updated_value = build_fdiv left_val right_val "fdivtmp" builder in
          ignore (build_store updated_value var_alloca builder);
          updated_value
      | Ast.Power, true, true ->
          let pow_func =
            match lookup_function "llvm.pow.f64" the_module with
            | Some func -> func
            | None ->
                let pow_type =
                  function_type f64_type [| f64_type; f64_type |]
                in
                declare_function "llvm.pow.f64" pow_type the_module
          in
          build_call pow_func [| left_val; right_val |] "powtmp" builder
      | Ast.Percent, true, true ->
          build_frem left_val right_val "fmodtmp" builder
      | Ast.Eq, true, true ->
          build_fcmp Fcmp.Oeq left_val right_val "feqtmp" builder
      | Ast.Neq, true, true ->
          build_fcmp Fcmp.One left_val right_val "fneqtmp" builder
      | Ast.Leq, true, true ->
          build_fcmp Fcmp.Ole left_val right_val "fleqtmp" builder
      | Ast.Geq, true, true ->
          build_fcmp Fcmp.Oge left_val right_val "fgeqtmp" builder
      | Ast.Less, true, true ->
          build_fcmp Fcmp.Olt left_val right_val "fletmp" builder
      | Ast.Greater, true, true ->
          build_fcmp Fcmp.Ogt left_val right_val "fgetmp" builder
      | Ast.LogicalAnd, true, true ->
          build_and left_val right_val "andtmp" builder
      | Ast.LogicalOr, true, true -> build_or left_val right_val "ortmp" builder
      | _ ->
          failwith
            "Codegen: Mixed or unsupported operand types for binary operation")
  | Expr.SizeofExpr { type_expr } ->
      let size_in_bytes = type_size_in_bytes type_expr in
      const_int i64_type size_in_bytes
  | Expr.VarExpr identifier -> (
      try
        let var_alloca = Hashtbl.find variables identifier in
        build_load var_alloca identifier builder
      with Not_found -> failwith ("Codegen: Unknown variable: " ^ identifier))
  | Expr.CallExpr { callee; arguments } -> (
      let func_name =
        match callee with
        | Expr.VarExpr name -> name
        | _ -> failwith "Codegen: Unsupported function call format"
      in
      match lookup_function func_name the_module with
      | Some func ->
          let arg_vals = List.map codegen_expr arguments in
          let arg_vals_array = Array.of_list arg_vals in
          let func_type = return_type (element_type (type_of func)) in
          if Array.length arg_vals_array != Array.length (params func) then
            failwith
              ("Codegen: Incorrect number of arguments for function "
             ^ func_name);
          if classify_type func_type = TypeKind.Void then
            build_call func arg_vals_array "" builder
          else build_call func arg_vals_array "calltmp" builder
      | None -> failwith ("Codegen: Unknown function " ^ func_name))
  | Expr.PrintlnExpr { expr } -> (
      let value =
        match expr with
        | Ast.Expr.FieldAccess { object_name; member_name } -> (
            match get_struct_instance_opt object_name with
            | Some struct_instance ->
                let _, fields = get_struct_type object_name in
                let field_index = find_field_index fields member_name in
                let field_ptr =
                  build_struct_gep struct_instance field_index "fieldptr"
                    builder
                in
                build_load field_ptr member_name builder
            | None -> (
                match
                  Hashtbl.find_opt enum_values (object_name ^ "." ^ member_name)
                with
                | Some enum_value -> enum_value
                | None ->
                    failwith
                      ("Codegen: '" ^ object_name ^ "." ^ member_name
                     ^ "' is neither a struct field nor an enum member")))
        | _ -> codegen_expr expr
      in
      let llvm_type = type_of value in
      match classify_type llvm_type with
      | TypeKind.Pointer ->
          let format_str = build_global_stringptr "%s\n" "str_fmt" builder in
          ignore
            (build_call printf_func [| format_str; value |] "printtmp" builder);
          value
      | _ -> print_any_type value llvm_type)
  | Expr.PrintlnFormatExpr { format_string; arguments } ->
      let process_format_string fmt_str args =
        let buffer = Buffer.create 16 in
        let llvm_args = ref [] in
        let arg_idx = ref 0 in
        let fmt_len = String.length fmt_str in
        let i = ref 0 in

        while !i < fmt_len do
          if
            !i < fmt_len - 1
            && String.get fmt_str !i = '%'
            && String.get fmt_str (!i + 1) = 'v'
          then (
            let value = codegen_expr (List.nth args !arg_idx) in
            let llvm_type = type_of value in
            llvm_args := !llvm_args @ [ value ];
            incr i;

            (match classify_type llvm_type with
            | TypeKind.Integer when is_char_type llvm_type ->
                Buffer.add_string buffer "%c"
            | TypeKind.Integer -> Buffer.add_string buffer "%d"
            | TypeKind.Float | TypeKind.Double -> Buffer.add_string buffer "%f"
            | TypeKind.Pointer -> Buffer.add_string buffer "%s"
            | _ -> Buffer.add_string buffer "%s");

            incr arg_idx)
          else Buffer.add_char buffer (String.get fmt_str !i);
          incr i
        done;

        (Buffer.contents buffer, Array.of_list !llvm_args)
      in

      let modified_format_str, llvm_args =
        process_format_string format_string arguments
      in
      let format_str =
        build_global_stringptr (modified_format_str ^ "\n") "fmt" builder
      in
      let all_args = Array.append [| format_str |] llvm_args in

      ignore (build_call printf_func all_args "printtmp" builder);
      format_str
  | Expr.UnaryExpr { operator; operand } -> (
      let var_val = codegen_expr operand in
      let is_float = classify_type (type_of var_val) = TypeKind.Double in
      let var_name =
        match operand with
        | Expr.VarExpr identifier -> identifier
        | _ ->
            failwith
              "Codegen: Increment/Decrement can only be applied to variables"
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
      | Ast.Minus ->
          if is_float then build_fneg var_val "fnegtmp" builder
          else build_neg var_val "negtmp" builder
      | _ -> failwith "Codegen: Unsupported unary operator")
  | Expr.CastExpr { expr; target_type } -> (
      let value = codegen_expr expr in
      let target_llvm_type =
        match target_type with
        | Ast.Type.SymbolType { value = "int" } -> i64_type
        | Ast.Type.SymbolType { value = "float" } -> f64_type
        | Ast.Type.SymbolType { value = "string" } -> string_type
        | _ -> failwith "Codegen: Unsupported target type for cast"
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
      | _ -> failwith "Codegen: Unsupported cast operation")
  | Expr.TypeofExpr { expr } ->
      let value = codegen_expr expr in
      let llvm_type_str = string_of_llvm_type (type_of value) in
      build_global_stringptr llvm_type_str "typestrtmp" builder
  | Expr.LengthExpr { expr } ->
      let value = codegen_expr expr in
      if classify_type (type_of value) <> TypeKind.Pointer then
        failwith "Codegen: LengthExpr is only valid for strings";
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
          let format_str = build_global_stringptr "%c\\0" "char_fmt" builder in
          ignore (build_call scanf_func [| format_str; char_ptr |] "" builder);
          build_load char_ptr "charresult" builder
      | Ast.Type.SymbolType { value = "string" } ->
          let str_ptr = build_alloca string_type "stringinput" builder in
          let format_str = build_global_stringptr "%s\\0" "str_fmt" builder in
          ignore (build_call scanf_func [| format_str; str_ptr |] "" builder);
          str_ptr
      | _ -> failwith "Codegen: Unsupported type for input")
  | Expr.AssignmentExpr { identifier; value = Some expr_value } -> (
      try
        let var_alloca = Hashtbl.find variables identifier in
        let new_value = codegen_expr expr_value in
        ignore (build_store new_value var_alloca builder);
        new_value
      with Not_found -> failwith ("Codegen: Unknown variable: " ^ identifier))
  | Expr.TernaryExpr { cond; onTrue; onFalse } ->
      let cond_val = codegen_expr cond in
      let zero = const_int i1_type 0 in
      let cond_bool = build_icmp Icmp.Ne cond_val zero "ifcond" builder in

      let current_bb = insertion_block builder in
      let the_function = block_parent current_bb in

      let then_bb = append_block context "then" the_function in
      let else_bb = append_block context "else" the_function in
      let merge_bb = append_block context "ifcont" the_function in
      let result_alloca =
        build_alloca (type_of (codegen_expr onTrue)) "result" builder
      in

      ignore (build_cond_br cond_bool then_bb else_bb builder);

      position_at_end then_bb builder;
      let then_val = codegen_expr onTrue in
      ignore (build_store then_val result_alloca builder);
      ignore (build_br merge_bb builder);

      position_at_end else_bb builder;
      let else_val = codegen_expr onFalse in
      ignore (build_store else_val result_alloca builder);
      ignore (build_br merge_bb builder);

      position_at_end merge_bb builder;
      build_load result_alloca "iftmp" builder
  | Expr.StructFieldAssign { struct_name; field_name; value } ->
      let struct_instance = get_struct_instance struct_name in
      let _, fields = get_struct_type struct_name in
      let field_index = find_field_index fields field_name in
      let value_code = codegen_expr value in
      let field_ptr =
        build_struct_gep struct_instance field_index "fieldptr" builder
      in
      ignore (build_store value_code field_ptr builder);
      value_code
  | _ -> failwith "Codegen: Expression not implemented"

let rec codegen_stmt = function
  | Stmt.ExprStmt expr -> codegen_expr expr
  | Stmt.EnumDeclStmt { name; members } ->
      let rec define_enum_members members index =
        match members with
        | [] -> ()
        | member_name :: rest ->
            let enum_const = const_int i64_type index in
            Hashtbl.add enum_values (name ^ "." ^ member_name) enum_const;
            define_enum_members rest (index + 1)
      in
      define_enum_members members 0;
      const_null i64_type
  | Stmt.StructStmt { name; fields; priv = _ } ->
      let field_list = snd fields in
      let llvm_struct_type = define_struct_type name field_list in
      let llvm_struct_value =
        build_alloca llvm_struct_type (name ^ "_instance") builder
      in
      Hashtbl.add variables name llvm_struct_value;
      llvm_struct_value
  | Stmt.VarDeclarationStmt
      { identifier; constant = _; assigned_value; explicit_type } ->
      let llvm_type =
        match explicit_type with
        | Ast.Type.SymbolType { value = "int" } -> i64_type
        | Ast.Type.SymbolType { value = "float" } -> f64_type
        | Ast.Type.SymbolType { value = "char" } -> char_type
        | Ast.Type.SymbolType { value = "string" } -> string_type
        | Ast.Type.SymbolType { value = "bool" } -> i1_type
        | Ast.Type.ArrayType { element } ->
            let elem_type =
              match element with
              | Ast.Type.SymbolType { value = "int" } -> i64_type
              | Ast.Type.SymbolType { value = "float" } -> f64_type
              | Ast.Type.SymbolType { value = "char" } -> char_type
              | Ast.Type.SymbolType { value = "string" } -> string_type
              | _ -> failwith "Codegen: Unsupported array element type"
            in
            pointer_type elem_type
        | _ -> failwith "Codegen: Unsupported type for variable declaration"
      in
      let alloca =
        match explicit_type with
        | Ast.Type.ArrayType { element = _ } ->
            let num_elements =
              match assigned_value with
              | Some (Ast.Expr.ArrayExpr { elements }) -> List.length elements
              | _ ->
                  failwith
                    "Codegen: Array requires initialization with elements"
            in
            let element_size = size_of llvm_type in
            let total_size =
              build_mul
                (const_int i64_type num_elements)
                element_size "array_size" builder
            in
            let malloc_fn = declare_malloc_function context the_module in
            build_call malloc_fn [| total_size |] "array_alloc" builder
        | _ -> build_alloca llvm_type identifier builder
      in
      Hashtbl.add variables identifier alloca;
      (match assigned_value with
      | Some expr -> (
          match explicit_type with
          | Ast.Type.ArrayType { element = _ } ->
              let initial_values =
                match expr with
                | Ast.Expr.ArrayExpr { elements } -> elements
                | _ ->
                    failwith
                      "Codegen: Expected array literal for array initialization"
              in
              let array_index = ref 0 in
              List.iter
                (fun elem_expr ->
                  let elem_value = codegen_expr elem_expr in
                  let index_ptr =
                    build_gep alloca
                      [| const_int i64_type !array_index |]
                      "index_ptr" builder
                  in
                  ignore (build_store elem_value index_ptr builder);
                  incr array_index)
                initial_values
          | _ ->
              let initial_value = codegen_expr expr in
              ignore (build_store initial_value alloca builder))
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
        | _ -> failwith "Codegen: Unsupported function return type"
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
               | _ -> failwith "Codegen: Unsupported parameter type")
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
  | _ -> failwith "Codegen: Statement not implemented"

let generate_code ast =
  ignore (codegen_stmt ast);
  print_module "a.out.ll" the_module
