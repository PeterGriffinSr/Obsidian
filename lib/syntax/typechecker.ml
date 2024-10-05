open Ast

module TypeChecker = struct
  module Env = Map.Make (String)

  type env = { var_type : Type.t Env.t }

  let empty_env = { var_type = Env.empty }

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
        { var_type = Env.add identifier explicit_type env.var_type }
      else
        failwith ("Type mismatch in variable declaration: " ^ identifier)
    | None -> failwith ("Variable " ^ identifier ^ " has no value assigned")

  let rec check_stmt env ~expected_return_type = function
    | Stmt.BlockStmt { body } -> check_block env body ~expected_return_type
    | Stmt.ExprStmt expr ->
        let _ = check_expr env expr in
        env
    | Stmt.VarDeclarationStmt { identifier; constant = _; assigned_value; explicit_type } ->
      check_variable_decl env identifier explicit_type assigned_value
    | _ -> failwith "Unsupported statement"

  and check_block env stmts ~expected_return_type =
    List.fold_left
      (fun env stmt -> check_stmt env stmt ~expected_return_type)
      env stmts
end
