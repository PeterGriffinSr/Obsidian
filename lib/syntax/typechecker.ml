open Ast

module TypeChecker = struct
  module Env = Map.Make (String)

  type func_sig = { param_type : Type.t list; return_type : Type.t }
  type env = { var_type : Type.t Env.t; func_type : func_sig Env.t }

  let empty_env = { var_type = Env.empty; func_type = Env.empty }

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
        | _ -> failwith "Unsupported operator in binary expression")
    | Expr.VarExpr name -> lookup_variables env name
    | _ -> failwith "Unsupported expression"

  let check_variable_decl env identifier explicit_type assigned_value =
    match assigned_value with
    | Some expr ->
        let value_type = check_expr env expr in
        if value_type = explicit_type then
          { env with var_type = Env.add identifier explicit_type env.var_type }
        else failwith ("Type mismatch in variable declaration: " ^ identifier)
    | None -> failwith ("Variable " ^ identifier ^ " has no value assigned")

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
    let new_env = { var_type = var_env; func_type } in
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
    | _ -> failwith "Unsupported statement"

  and check_block env stmts ~expected_return_type =
    List.fold_left
      (fun env stmt -> check_stmt env stmt ~expected_return_type)
      env stmts
end
