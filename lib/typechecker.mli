exception TypeError of string
exception UnboundFunctionError of string
exception UnboundVariableError of string
exception UnsupportedOperationError of string
exception ArgumentMismatchError of string
exception ReturnTypeError of string

module TypeChecker : sig
  module Env : sig
    type key = string
    type 'a t = 'a Map.Make(String).t

    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t

    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  type func_sig = { param_type : Ast.Type.t list; return_type : Ast.Type.t }

  type env = {
    var_type : Ast.Type.t Env.t;
    func_type : func_sig Env.t;
    enum_type : (string * Ast.Type.t) list Env.t;
    struct_types : (string * (string * Ast.Type.t) list) Env.t;
  }

  val empty_env : env
  val lookup_function : env -> Env.key -> func_sig
  val lookup_variables : env -> Env.key -> Ast.Type.t
  val raise_type_mismatch_error : Ast.Type.t -> Ast.Type.t -> 'a
  val count_format_specifiers : string -> int
  val is_valid_type : env -> Ast.Type.t -> bool

  val check_struct_decl :
    env -> Env.key * (string * (string * Ast.Type.t) list) -> env

  val check_format_string_arguments : env -> string -> Ast.Expr.t list -> unit
  val check_expr : env -> Ast.Expr.t -> Ast.Type.t
  val check_enum_decl : env -> Env.key -> string list -> env

  val check_variable_decl :
    env -> Env.key -> Ast.Type.t -> Ast.Expr.t option -> env

  val check_func_decl :
    env ->
    Env.key ->
    Ast.Stmt.parameter list ->
    Ast.Type.t ->
    Ast.Stmt.t list ->
    env

  val check_stmt :
    env -> expected_return_type:Ast.Type.t option -> Ast.Stmt.t -> env

  val check_block :
    env -> Ast.Stmt.t list -> expected_return_type:Ast.Type.t option -> env
end
