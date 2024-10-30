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

let pp_token fmt = function
  | Plus -> Format.fprintf fmt "Plus"
  | Minus -> Format.fprintf fmt "Minus"
  | Star -> Format.fprintf fmt "Star"
  | Slash -> Format.fprintf fmt "Slash"
  | Percent -> Format.fprintf fmt "Percent"
  | Power -> Format.fprintf fmt "Power"
  | Carot -> Format.fprintf fmt "Carot"
  | LParen -> Format.fprintf fmt "LParen"
  | RParen -> Format.fprintf fmt "RParen"
  | LBracket -> Format.fprintf fmt "LBracket"
  | RBracket -> Format.fprintf fmt "RBracket"
  | LBrace -> Format.fprintf fmt "LBrace"
  | RBrace -> Format.fprintf fmt "RBrace"
  | Dot -> Format.fprintf fmt "Dot"
  | Colon -> Format.fprintf fmt "Colon"
  | Semi -> Format.fprintf fmt "Semi"
  | Comma -> Format.fprintf fmt "Comma"
  | Not -> Format.fprintf fmt "Not"
  | Ampersand -> Format.fprintf fmt "Ampersand"
  | Greater -> Format.fprintf fmt "Greater"
  | Less -> Format.fprintf fmt "Less"
  | LogicalOr -> Format.fprintf fmt "LogicalOr"
  | LogicalAnd -> Format.fprintf fmt "LogicalAnd"
  | Eq -> Format.fprintf fmt "Eq"
  | Neq -> Format.fprintf fmt "Neq"
  | Geq -> Format.fprintf fmt "Geq"
  | Leq -> Format.fprintf fmt "Leq"
  | Dec -> Format.fprintf fmt "Dec"
  | Inc -> Format.fprintf fmt "Inc"
  | Assign -> Format.fprintf fmt "Assign"
  | Fn -> Format.fprintf fmt "Function"
  | If -> Format.fprintf fmt "If"
  | Else -> Format.fprintf fmt "Else"
  | Return -> Format.fprintf fmt "Return"
  | Const -> Format.fprintf fmt "Const"
  | Switch -> Format.fprintf fmt "Switch"
  | Case -> Format.fprintf fmt "Case"
  | Break -> Format.fprintf fmt "Break"
  | Default -> Format.fprintf fmt "Default"
  | While -> Format.fprintf fmt "While"
  | For -> Format.fprintf fmt "For"
  | Import -> Format.fprintf fmt "Import"
  | Export -> Format.fprintf fmt "Export"
  | Class -> Format.fprintf fmt "Class"
  | New -> Format.fprintf fmt "New"
  | Null -> Format.fprintf fmt "Null"
  | Cast -> Format.fprintf fmt "Cast"
  | Alloc -> Format.fprintf fmt "Alloc"
  | Dealloc -> Format.fprintf fmt "Dealloc"
  | Sizeof -> Format.fprintf fmt "Sizeof"
  | Typeof -> Format.fprintf fmt "Typeof"
  | Enum -> Format.fprintf fmt "Enum"
  | Unsafe -> Format.fprintf fmt "Unsafe"
  | Public -> Format.fprintf fmt "Public"
  | Private -> Format.fprintf fmt "Private"
  | This -> Format.fprintf fmt "This"
  | Println -> Format.fprintf fmt "Println"
  | MinusAssign -> Format.fprintf fmt "MinusAssign"
  | PlusAssign -> Format.fprintf fmt "PlusAssign"
  | StarAssign -> Format.fprintf fmt "StarAssign"
  | SlashAssign -> Format.fprintf fmt "SlashAssign"
  | Pipe -> Format.fprintf fmt "Pipe"
  | Leftshift -> Format.fprintf fmt "Leftshift"
  | Rightshift -> Format.fprintf fmt "Rightshift"
  | Xor -> Format.fprintf fmt "Xor"
  | Identifier s -> Format.fprintf fmt "Identifier(%s)" s
  | Int i -> Format.fprintf fmt "Int(%d)" i
  | Float f -> Format.fprintf fmt "Float(%f)" f
  | String s -> Format.fprintf fmt "String(%s)" s
  | Char c -> Format.fprintf fmt "Char(%c)" c
  | Bool b -> Format.fprintf fmt "Bool(%b)" b
  | Void v -> Format.fprintf fmt "Void(%s)" v
  | EOF -> Format.fprintf fmt "EOF"

module Type = struct
  type t =
    | SymbolType of { value : string }
    | PointerType of { base_type : t }
    | ArrayType of { element : t }
  [@@deriving show]
end

module Expr = struct
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
  [@@deriving show]
end

module Stmt = struct
  type parameter = { name : string; param_type : Type.t } [@@deriving show]

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
  [@@deriving show]
end
