open Ast

module TypeChecker = struct
  module Env = Map.Make (String)

  type func_sig = { param_type : Type.t list; return_type : Type.t }

  type env = {
    var_type : Type.t Env.t;
    func_type : func_sig Env.t;
    enum_type : (string * Type.t) list Env.t;
  }

  let empty_env =
    let println_sig =
      { param_type = []; return_type = Type.SymbolType { value = "void" } }
    in
    {
      var_type = Env.empty;
      func_type = Env.add "println" println_sig Env.empty;
      enum_type = Env.empty;
    }

  let lookup_function env name =
    try Env.find name env.func_type
    with Not_found -> failwith ("Unbound function: " ^ name)

  let lookup_variables env name =
    try Env.find name env.var_type
    with Not_found -> failwith ("Unbound variable: " ^ name)

  let rec check_expr env = function
    | Expr.IntExpr _ -> Type.SymbolType { value = "int" }
    | Expr.FloatExpr _ -> Type.SymbolType { value = "float" }
    | Expr.StringExpr _ -> Type.SymbolType { value = "string" }
    | Expr.CharExpr _ -> Type.SymbolType { value = "char" }
    | Expr.BoolExpr _ -> Type.SymbolType { value = "bool" }
    | Expr.BinaryExpr { left; operator; right } -> (
        let left_type = check_expr env left in
        let right_type = check_expr env right in
        match operator with
        | Plus | Minus | Star | Slash | Percent | Power ->
            if left_type = right_type then left_type
            else failwith "Type mismatch in arithmetic expression"
        | Eq | Neq | Less | Greater | Leq | Geq ->
            if left_type = right_type then Type.SymbolType { value = "bool" }
            else failwith "Type mismatch in comparison expression"
        | Carot ->
            if
              left_type = Type.SymbolType { value = "string" }
              && right_type = Type.SymbolType { value = "string" }
            then Type.SymbolType { value = "string" }
            else
              failwith
                "TypeChecker: Type mismatch in string concatenation, both \
                 operands must be strings"
        | LogicalAnd | LogicalOr ->
            if
              (left_type = Type.SymbolType { value = "bool" }
              || left_type = Type.SymbolType { value = "int" })
              && (left_type = Type.SymbolType { value = "bool" }
                 || right_type = Type.SymbolType { value = "int" })
            then Ast.Type.SymbolType { value = "bool" }
            else failwith "TypeChecker: Type mismatch in logical expression"
        | _ -> failwith "Unsupported operator in binary expression")
    | Expr.VarExpr name -> lookup_variables env name
    | Expr.CallExpr { callee; arguments } ->
        let func_name =
          match callee with
          | Expr.VarExpr name -> name
          | _ -> failwith "TypeChecker: Unsupported function call"
        in
        let { param_type; return_type } = lookup_function env func_name in
        if List.length arguments <> List.length param_type then
          failwith
            ("TypeChecker: Incorrect number of arguments for function "
           ^ func_name);
        List.iter2
          (fun arg param_type ->
            let arg_type = check_expr env arg in
            if arg_type <> param_type then
              failwith
                ("TypeChecker: Argument type mismatch in call to function "
               ^ func_name))
          arguments param_type;
        return_type
    | Expr.UnaryExpr { operator; operand } -> (
        let operand_type = check_expr env operand in
        match operator with
        | Ast.Not ->
            if operand_type = Ast.Type.SymbolType { value = "bool" } then
              Ast.Type.SymbolType { value = "bool" }
            else
              failwith "TypeChecker: Operand of NOT operator must be a boolean"
        | Ast.Inc | Ast.Dec ->
            if
              operand_type = Ast.Type.SymbolType { value = "int" }
              || operand_type = Ast.Type.SymbolType { value = "float" }
            then operand_type
            else
              failwith
                "TypeChecker: Operand of INC/DEC must be an integer or float"
        | _ -> failwith "TypeChecker: Unsupported unary operator")
    | Expr.PrintlnExpr { expr } ->
        let _ = check_expr env expr in
        Type.SymbolType { value = "void" }
    | Expr.LengthExpr { expr } ->
        let expr_type = check_expr env expr in
        if expr_type = Type.SymbolType { value = "string" } then
          Type.SymbolType { value = "int" }
        else failwith "TypeChecker: length can only be applied to a string"
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
            failwith
              ("TypeChecker: Unsupported cast from " ^ Type.show source_type
             ^ " to " ^ Type.show target_type))
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
        | _ -> failwith "TypeChecker: Unsupported type for sizeof")
    | Expr.InputExpr { prompt = _; target_type } -> (
        match target_type with
        | Type.SymbolType { value = "int" }
        | Type.SymbolType { value = "float" }
        | Type.SymbolType { value = "char" }
        | Type.SymbolType { value = "string" } ->
            target_type
        | _ -> failwith "TypeChecker: Unsupported type for input")
    | _ -> failwith "Unsupported expression"

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
        else failwith ("Type mismatch in variable declaration: " ^ identifier)
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
      { var_type = var_env; func_type; enum_type = env.enum_type }
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
          | None -> failwith ("Function " ^ name ^ " must have a return type")
        in
        check_func_decl env name parameters return_type body
    | Stmt.ReturnStmt expr -> (
        let return_type = check_expr env expr in
        match expected_return_type with
        | Some expected_type ->
            if return_type <> expected_type then
              failwith
                ("Return type mismatch: expected " ^ Type.show expected_type
               ^ ", got " ^ Type.show return_type)
            else env
        | None -> env)
    | Stmt.IfStmt { condition; then_branch; else_branch } ->
        let cond_type = check_expr env condition in
        if cond_type <> Ast.Type.SymbolType { value = "bool" } then
          failwith "TypeChecker: Condition in if statement must be a boolean"
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
              failwith
                "TypeChecker: Case expression type does not match switch \
                 expression";
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
            failwith "TypeChecker: Condition in for statement must be a boolean"
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
            failwith "TypeChecker: Condition in for statement must be a boolean"
        in
        check_block env body ~expected_return_type
    | Stmt.EnumDeclStmt { name; members } -> check_enum_decl env name members
    | _ -> failwith "Unsupported statement"

  and check_block env stmts ~expected_return_type =
    List.fold_left
      (fun env stmt -> check_stmt env stmt ~expected_return_type)
      env stmts
end
