open Ast

exception TypeError of string
exception UnboundFunctionError of string
exception UnboundVariableError of string
exception UnsupportedOperationError of string
exception ArgumentMismatchError of string
exception ReturnTypeError of string

module TypeChecker = struct
  module Env = Map.Make (String)

  type func_sig = { param_type : Type.t list; return_type : Type.t }

  type env = {
    var_type : Type.t Env.t;
    func_type : func_sig Env.t;
    enum_type : (string * Type.t) list Env.t;
    struct_types : (string * (string * Type.t) list) Env.t;
  }

  let empty_env =
    let println_sig =
      { param_type = []; return_type = Type.SymbolType { value = "void" } }
    in
    {
      var_type = Env.empty;
      func_type = Env.add "println" println_sig Env.empty;
      enum_type = Env.empty;
      struct_types = Env.empty;
    }

  let lookup_function env name =
    try Env.find name env.func_type
    with Not_found ->
      raise (UnboundFunctionError ("Typechecker: Unbound function: " ^ name))

  let lookup_variables env name =
    try Env.find name env.var_type
    with Not_found ->
      raise (UnboundVariableError ("Typechecker: Unbound variable: " ^ name))

  let raise_type_mismatch_error expected actual =
    raise
      (TypeError
         ("Type mismatch: expected " ^ Type.show expected ^ ", got "
        ^ Type.show actual))

  and count_format_specifiers format_string =
    let rec count format_string index acc =
      if index >= String.length format_string then acc
      else if
        format_string.[index] = '%'
        && index + 1 < String.length format_string
        && format_string.[index + 1] = 'v'
      then count format_string (index + 2) (acc + 1)
      else count format_string (index + 1) acc
    in
    count format_string 0 0

  let rec is_valid_type env = function
    | Type.SymbolType { value } -> (
        match value with
        | "int" | "float" | "string" | "char" | "bool" | "void" -> true
        | _ -> Env.mem value env.struct_types || Env.mem value env.enum_type)
    | Type.ArrayType { element } -> is_valid_type env element
    | _ -> false

  let check_struct_decl env (name, fields) =
    if Env.mem name env.struct_types then
      raise (TypeError ("Struct " ^ name ^ " is already defined"));
    { env with struct_types = Env.add name fields env.struct_types }

  let rec check_format_string_arguments env format_string arguments =
    let num_specifiers = count_format_specifiers format_string in
    if num_specifiers <> List.length arguments then
      raise
        (ArgumentMismatchError
           "Typechecker: Number of format specifiers does not match the number \
            of arguments");
    List.iter (fun arg -> ignore (check_expr env arg)) arguments

  and check_expr env = function
    | Expr.IntExpr _ -> Type.SymbolType { value = "int" }
    | Expr.FloatExpr _ -> Type.SymbolType { value = "float" }
    | Expr.StringExpr _ -> Type.SymbolType { value = "string" }
    | Expr.CharExpr _ -> Type.SymbolType { value = "char" }
    | Expr.BoolExpr _ -> Type.SymbolType { value = "bool" }
    | Expr.BinaryExpr { left; operator; right } -> (
        let left_type = check_expr env left in
        let right_type = check_expr env right in
        match operator with
        | Plus | Minus | Star | Slash | Percent | Power | PlusAssign
        | MinusAssign | StarAssign | SlashAssign | Ampersand | Pipe | Rightshift
        | Leftshift | Xor ->
            if left_type = right_type then left_type
            else raise_type_mismatch_error left_type right_type
        | Eq | Neq | Less | Greater | Leq | Geq ->
            if left_type = right_type then
              match left_type with
              | Type.SymbolType { value = "int" }
              | Type.SymbolType { value = "float" }
              | Type.SymbolType { value = "char" }
              | Type.SymbolType { value = "string" } ->
                  Type.SymbolType { value = "bool" }
              | _ ->
                  raise
                    (UnsupportedOperationError
                       "Typechecker: Unsupported type for comparison")
            else raise_type_mismatch_error left_type right_type
        | LogicalAnd | LogicalOr ->
            (* Printf.printf "Evaluating left operand: %s\n" (Type.show left_type);
               Printf.printf "Evaluating right operand: %s\n"
                 (Type.show right_type); *)
            if
              left_type = Type.SymbolType { value = "bool" }
              && right_type = Type.SymbolType { value = "bool" }
            then Type.SymbolType { value = "bool" }
            else
              raise
                (TypeError
                   "Typechecker: Logical operators require both operands to be \
                    boolean")
        | Carot ->
            if
              left_type = Type.SymbolType { value = "string" }
              && right_type = Type.SymbolType { value = "string" }
            then Type.SymbolType { value = "string" }
            else
              raise
                (TypeError
                   "Typechecker: String concatenation requires both operands \
                    to be strings")
        | _ ->
            raise
              (UnsupportedOperationError
                 "Typechecker: Unsupported operator in binary expression"))
    | Expr.FieldAccess { object_name; member_name } -> (
        match Env.find_opt object_name env.struct_types with
        | Some (_, fields) -> (
            match List.assoc_opt member_name fields with
            | Some field_type -> field_type
            | None ->
                raise
                  (TypeError
                     ("Typechecker: Field '" ^ member_name
                    ^ "' not found in struct '" ^ object_name ^ "'")))
        | None -> (
            match Env.find_opt object_name env.enum_type with
            | Some members -> (
                match List.assoc_opt member_name members with
                | Some enum_type -> enum_type
                | None ->
                    raise
                      (TypeError
                         ("Typechecker: Enum member '" ^ member_name
                        ^ "' not found in enum '" ^ object_name ^ "'")))
            | None ->
                raise
                  (TypeError
                     ("Typechecker: '" ^ object_name
                    ^ "' is neither a struct nor an enum"))))
    | Expr.VarExpr name -> lookup_variables env name
    | Expr.CallExpr { callee; arguments } ->
        let func_name =
          match callee with
          | Expr.VarExpr name -> name
          | _ -> raise (TypeError "Typechecker: Unsupported function call")
        in
        let { param_type; return_type } = lookup_function env func_name in
        if List.length arguments <> List.length param_type then
          raise
            (ArgumentMismatchError
               ("Typechecker: Incorrect number of arguments for function "
              ^ func_name));
        List.iter2
          (fun arg param_type ->
            let arg_type = check_expr env arg in
            if arg_type <> param_type then
              raise_type_mismatch_error param_type arg_type)
          arguments param_type;
        return_type
    | Expr.UnaryExpr { operator; operand } -> (
        let operand_type = check_expr env operand in
        match operator with
        | Ast.Not ->
            if operand_type = Ast.Type.SymbolType { value = "bool" } then
              Ast.Type.SymbolType { value = "bool" }
            else
              raise
                (TypeError
                   "Typechecker: Operand of NOT operator must be a boolean")
        | Ast.Inc | Ast.Dec ->
            if
              operand_type = Ast.Type.SymbolType { value = "int" }
              || operand_type = Ast.Type.SymbolType { value = "float" }
            then operand_type
            else
              raise
                (TypeError "Typechecker: INC/DEC requires int or float operand")
        | Ast.Minus ->
            if
              operand_type = Ast.Type.SymbolType { value = "int" }
              || operand_type = Ast.Type.SymbolType { value = "float" }
            then operand_type
            else
              raise
                (TypeError "Typechecker: MINUS requires int or float operand")
        | _ -> raise (UnsupportedOperationError "Unsupported unary operator"))
    | Expr.PrintlnExpr { expr } ->
        let _ = check_expr env expr in
        Type.SymbolType { value = "void" }
    | Expr.PrintlnFormatExpr { format_string; arguments } ->
        check_format_string_arguments env format_string arguments;
        Type.SymbolType { value = "void" }
    | Expr.LengthExpr { expr } ->
        let expr_type = check_expr env expr in
        if expr_type = Type.SymbolType { value = "string" } then
          Type.SymbolType { value = "int" }
        else
          raise (TypeError "Typechecker: Length can only be applied to strings")
    | Expr.CastExpr { expr; target_type } -> (
        let source_type = check_expr env expr in
        match (source_type, target_type) with
        | Type.SymbolType { value = "int" }, Type.SymbolType { value = "float" }
        | Type.SymbolType { value = "float" }, Type.SymbolType { value = "int" }
        | ( Type.SymbolType { value = "int" },
            Type.SymbolType { value = "string" } )
        | ( Type.SymbolType { value = "float" },
            Type.SymbolType { value = "string" } )
        | Type.SymbolType { value = "bool" }, Type.SymbolType { value = "int" }
        | ( Type.SymbolType { value = "bool" },
            Type.SymbolType { value = "string" } )
        | ( Type.SymbolType { value = "char" },
            Type.SymbolType { value = "string" } ) ->
            target_type
        | _ when source_type = target_type -> target_type
        | _ ->
            raise
              (TypeError
                 ("Typechecker: Unsupported cast from " ^ Type.show source_type
                ^ " to " ^ Type.show target_type)))
    | Expr.TypeofExpr { expr } ->
        let _ = check_expr env expr in
        Type.SymbolType { value = "string" }
    | Expr.SizeofExpr { type_expr } -> (
        match type_expr with
        | Ast.Type.SymbolType { value = "int" }
        | Ast.Type.SymbolType { value = "float" }
        | Ast.Type.SymbolType { value = "char" }
        | Ast.Type.SymbolType { value = "string" }
        | Ast.Type.SymbolType { value = "bool" } ->
            Type.SymbolType { value = "int" }
        | _ ->
            raise
              (UnsupportedOperationError
                 "Typechecker: Unsupported type for sizeof"))
    | Expr.InputExpr { prompt = _; target_type } -> (
        match target_type with
        | Type.SymbolType { value = "int" }
        | Type.SymbolType { value = "float" }
        | Type.SymbolType { value = "char" }
        | Type.SymbolType { value = "string" } ->
            target_type
        | _ -> raise (TypeError "Typechecker: Unsupported type for input"))
    | Expr.AssignmentExpr { identifier; value = Some expr_value } ->
        let var_type = lookup_variables env identifier in
        let assigned_type = check_expr env expr_value in
        if var_type <> assigned_type then
          raise_type_mismatch_error var_type assigned_type
        else var_type
    | Expr.ArrayExpr { elements } -> (
        match elements with
        | [] ->
            raise (TypeError "Typechecker: Cannot infer type of an empty array")
        | first_elem :: _ ->
            let elem_type = check_expr env first_elem in
            List.iter
              (fun elem ->
                let t = check_expr env elem in
                if t <> elem_type then
                  raise (TypeError "Type mismatch in array elements"))
              elements;
            Type.ArrayType { element = elem_type })
    | Expr.IndexExpr { array; index } -> (
        let array_type = check_expr env array in
        let index_type = check_expr env index in
        if index_type <> Type.SymbolType { value = "int" } then
          raise (TypeError "Typechecker: Array index must be an integer");
        match array_type with
        | Type.ArrayType { element } -> element
        | _ -> raise (TypeError "Typechecker: Cannot index non-array type"))
    | Expr.TernaryExpr { cond; onTrue; onFalse } ->
        let cond_type = check_expr env cond in
        if cond_type <> Type.SymbolType { value = "bool" } then
          raise
            (TypeError
               "Typechecker: Condition in ternary expression must be a boolean");
        let true_type = check_expr env onTrue in
        let false_type = check_expr env onFalse in
        if true_type = false_type then true_type
        else raise_type_mismatch_error true_type false_type
    | Expr.StructFieldAssign { struct_name; field_name; value } ->
        let struct_info =
          try Env.find struct_name env.struct_types
          with Not_found ->
            raise (TypeError ("Undefined struct type: " ^ struct_name))
        in
        let _, fields = struct_info in
        let field_type =
          match List.assoc_opt field_name fields with
          | Some t -> t
          | None ->
              raise
                (TypeError
                   ("Undefined field " ^ field_name ^ " in struct "
                  ^ struct_name))
        in
        let value_type = check_expr env value in
        if value_type <> field_type then
          raise_type_mismatch_error field_type value_type;
        Type.SymbolType { value = struct_name }
    | _ ->
        raise (UnsupportedOperationError "Typechecker: Unsupported expression")

  let check_enum_decl env name members =
    let enum_type =
      List.map
        (fun member -> (member, Type.SymbolType { value = name }))
        members
    in
    { env with enum_type = Env.add name enum_type env.enum_type }

  let check_variable_decl env identifier explicit_type assigned_value =
    match assigned_value with
    | Some expr ->
        let value_type = check_expr env expr in
        if value_type = explicit_type then
          { env with var_type = Env.add identifier explicit_type env.var_type }
        else
          raise
            (TypeError ("Type mismatch in variable declaration: " ^ identifier))
    | None ->
        { env with var_type = Env.add identifier explicit_type env.var_type }

  let rec check_func_decl env name parameters return_type body =
    let param_type =
      List.map (fun param -> param.Ast.Stmt.param_type) parameters
    in
    let var_env =
      List.fold_left
        (fun var_env param ->
          Env.add param.Ast.Stmt.name param.Ast.Stmt.param_type var_env)
        env.var_type parameters
    in
    let func_sig = { param_type; return_type } in
    let func_type = Env.add name func_sig env.func_type in
    let new_env =
      {
        var_type = var_env;
        func_type;
        enum_type = env.enum_type;
        struct_types = env.struct_types;
      }
    in
    let _ = check_block new_env body ~expected_return_type:(Some return_type) in
    { env with func_type }

  and check_stmt env ~expected_return_type = function
    | Stmt.BlockStmt { body } -> check_block env body ~expected_return_type
    | Stmt.ExprStmt expr ->
        let _ = check_expr env expr in
        env
    | Stmt.VarDeclarationStmt
        { identifier; constant = _; assigned_value; explicit_type } ->
        check_variable_decl env identifier explicit_type assigned_value
    | Stmt.FuncDeclStmt { name; parameters; return_type; body } ->
        let return_type =
          match return_type with
          | Some t -> t
          | None ->
              raise
                (ReturnTypeError
                   ("Typechecker: Function " ^ name ^ " must have a return type"))
        in
        check_func_decl env name parameters return_type body
    | Stmt.ReturnStmt expr -> (
        let return_type = check_expr env expr in
        match expected_return_type with
        | Some expected_type ->
            if return_type <> expected_type then
              raise
                (ReturnTypeError
                   ("Typechecker: Return type mismatch: expected "
                  ^ Type.show expected_type ^ ", got " ^ Type.show return_type))
            else env
        | None -> env)
    | Stmt.IfStmt { condition; then_branch; else_branch } ->
        let cond_type = check_expr env condition in
        if cond_type <> Ast.Type.SymbolType { value = "bool" } then
          raise
            (TypeError
               "Typechecker: Condition in if statement must be a boolean")
        else
          let env_then = check_stmt env ~expected_return_type then_branch in
          let env_final =
            match else_branch with
            | Some else_branch ->
                check_stmt env_then ~expected_return_type else_branch
            | None -> env_then
          in
          env_final
    | Stmt.SwitchStmt { expr; cases; default_case } ->
        let switch_type = check_expr env expr in
        List.iter
          (fun (case_expr, case_body) ->
            let case_type = check_expr env case_expr in
            if case_type <> switch_type then
              raise
                (TypeError
                   "Typechecker: Case expression type does not match switch \
                    expression");
            ignore (check_block env case_body ~expected_return_type))
          cases;
        (match default_case with
        | Some body -> ignore (check_block env body ~expected_return_type)
        | None -> ());
        env
    | Stmt.ForStmt { initialization; condition; iteration; body } ->
        let env =
          match initialization with
          | Some stmt -> check_stmt env ~expected_return_type:None stmt
          | None -> env
        in
        let _ =
          let cond_type = check_expr env condition in
          if cond_type <> Ast.Type.SymbolType { value = "bool" } then
            raise
              (TypeError
                 "Typechecker: Condition in for statement must be a boolean")
        in
        let env =
          match iteration with
          | Some stmt -> check_stmt env ~expected_return_type:None stmt
          | None -> env
        in
        check_block env [ body ] ~expected_return_type
    | Stmt.WhileStmt { expr; body } ->
        let _ =
          let cond_type = check_expr env expr in
          if cond_type <> Ast.Type.SymbolType { value = "bool" } then
            raise
              (TypeError
                 "Typechecker: Condition in while statement must be a boolean")
        in
        check_block env body ~expected_return_type
    | Stmt.EnumDeclStmt { name; members } -> check_enum_decl env name members
    | Stmt.StructStmt { name; fields; priv = _ } ->
        check_struct_decl env (name, fields)
    | _ ->
        raise (UnsupportedOperationError "Typechecker: Unsupported statement")

  and check_block env stmts ~expected_return_type =
    List.fold_left
      (fun env stmt -> check_stmt env stmt ~expected_return_type)
      env stmts
end
