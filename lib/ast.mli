type token =
  | Const
  | Fn
  | If
  | Else
  | Switch
  | Case
  | Default
  | Break
  | While
  | For
  | Return
  | Class
  | Enum
  | New
  | Null
  | Alloc
  | Dealloc
  | Sizeof
  | Unsafe
  | Public
  | Private
  | Typeof
  | Import
  | Export
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Comma
  | Dot
  | Semi
  | Colon
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Not
  | Assign
  | Less
  | Greater
  | Ampersand
  | Carot
  | Neq
  | Eq
  | Leq
  | Geq
  | LogicalAnd
  | LogicalOr
  | Inc
  | Dec
  | Power
  | Cast
  | This
  | Println
  | MinusAssign
  | PlusAssign
  | StarAssign
  | SlashAssign
  | Pipe
  | Leftshift
  | Rightshift
  | Xor
  | Identifier of string
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Void of string
  | EOF

val pp_token : Format.formatter -> token -> unit

module Type : sig
  type t =
    | SymbolType of { value : string }
    | PointerType of { base_type : t }
    | ArrayType of { element : t }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Expr : sig
  type t =
    | IntExpr of { value : int }
    | FloatExpr of { value : float }
    | StringExpr of { value : string }
    | CharExpr of { value : char }
    | BoolExpr of { value : bool }
    | VarExpr of string
    | BinaryExpr of { left : t; operator : token; right : t }
    | TernaryExpr of { cond : t; onTrue : t; onFalse : t }
    | UnaryExpr of { operator : token; operand : t }
    | CallExpr of { callee : t; arguments : t list }
    | AllocExpr of { size : t }
    | DeallocExpr of { pointer : t }
    | SizeofExpr of { type_expr : Type.t }
    | CastExpr of { expr : t; target_type : Type.t }
    | TypeofExpr of { expr : t }
    | LengthExpr of { expr : t }
    | PrintlnExpr of { expr : t }
    | PrintlnFormatExpr of { format_string : string; arguments : t list }
    | InputExpr of { prompt : string; target_type : Type.t }
    | NewExpr of { class_name : string }
    | MethodCall of { obj : t; method_name : string; arguments : t list }
    | AssignmentExpr of { identifier : string; value : t option }
    | ArrayExpr of { elements : t list }
    | IndexExpr of { array : t; index : t }
    | StructFieldAssign of {
        struct_name : string;
        field_name : string;
        value : t;
      }
    | FieldAccess of { object_name : string; member_name : string }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Stmt : sig
  type parameter = { name : string; param_type : Type.t }

  val pp_parameter : Format.formatter -> parameter -> unit
  val show_parameter : parameter -> string

  type t =
    | BlockStmt of { body : t list }
    | VarDeclarationStmt of {
        identifier : string;
        constant : bool;
        assigned_value : Expr.t option;
        explicit_type : Type.t;
      }
    | FuncDeclStmt of {
        name : string;
        parameters : parameter list;
        return_type : Type.t option;
        body : t list;
      }
    | WhileStmt of { expr : Expr.t; body : t list }
    | IfStmt of { condition : Expr.t; then_branch : t; else_branch : t option }
    | SwitchStmt of {
        expr : Expr.t;
        cases : (Expr.t * t list) list;
        default_case : t list option;
      }
    | ForStmt of {
        initialization : t option;
        condition : Expr.t;
        iteration : t option;
        body : t;
      }
    | EnumDeclStmt of { name : string; members : string list }
    | UnsafeStmt of { body : t list }
    | ImportStmt of { module_name : string }
    | ExportStmt of { identifier : string }
    | StructStmt of {
        name : string;
        fields : string * (string * Type.t) list;
        priv : bool;
      }
    | ExprStmt of Expr.t
    | ReturnStmt of Expr.t
    | BreakStmt

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
