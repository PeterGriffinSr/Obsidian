%left Plus Minus Carot
%left Star Slash Percent
%left Power

%token Int Float String Bool Char Void Const Fn If Else Switch Case Default Break While For Return Enum New Null Alloc Dealloc Sizeof Unsafe Private Typeof Import Export LParen RParen LBrace RBrace LBracket RBracket Comma Dot Semi Colon Plus Minus Star Slash Percent Not Assign Less Greater Ampersand Carot Neq Eq Leq Geq LogicalAnd LogicalOr Inc Dec Power Cast Println Length Input PlusAssign MinusAssign StarAssign SlashAssign Pipe Leftshift Rightshift Xor Question Struct

%token <string> Identifier
%token <int> IntLit
%token <float> FloatLit
%token <string> StringLit
%token <char> CharLit
%token <bool> BoolLit
%token EOF

%start program
%type <Ast.Stmt.t> program

%%

program:
    | stmt_list EOF { Ast.Stmt.BlockStmt { body = $1 } }

stmt_list:
    | stmt stmt_list { $1 :: $2 }
    | stmt { [$1] }

stmt:
    | simple_stmt Semi { $1 }
    | compound_stmt { $1 }

simple_stmt:
    | VarDeclStmt { $1 }
    | ReturnStmt { $1 }
    | ImportStmt { $1 }
    | ExportStmt { $1 }
    | expr { Ast.Stmt.ExprStmt $1 }

compound_stmt:
    | FuncDeclStmt { $1 }
    | IfStmt { $1 }
    | SwitchStmt { $1 }
    | WhileStmt { $1 }
    | ForStmt { $1 }
    | EnumStmt { $1 }
    | UnsafeStmt { $1 }
    | StructStmt { $1 }

type_expr:
    | Int { Ast.Type.SymbolType { value = "int" } }
    | Float { Ast.Type.SymbolType { value = "float" } }
    | String { Ast.Type.SymbolType { value = "string" } }
    | Char { Ast.Type.SymbolType { value = "char" } }
    | Bool { Ast.Type.SymbolType { value = "bool" } }
    | Void { Ast.Type.SymbolType { value = "void" } }
    | Star type_expr { Ast.Type.PointerType { base_type = $2 } } 
    | type_expr Star { Ast.Type.PointerType { base_type = $1 } }
    | LBracket RBracket type_expr { Ast.Type.ArrayType { element = $3 } }
    | type_expr LBracket RBracket { Ast.Type.ArrayType { element = $1 } }

expr:
    | expr Plus expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Plus; right = $3 } }
    | expr Minus expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Minus; right = $3 } }
    | expr Star expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Star; right = $3 } }
    | expr Slash expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Slash; right = $3 } }
    | expr Percent expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Percent; right = $3 } }
    | expr Power expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Power; right = $3 } }
    | expr Greater expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Greater; right = $3 } }
    | expr Less expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Less; right = $3 } }
    | expr LogicalOr expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.LogicalOr; right = $3 } }
    | expr LogicalAnd expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.LogicalAnd; right = $3 } }
    | expr Eq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Eq; right = $3 } }
    | expr Neq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Neq; right = $3 } }
    | expr Geq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Geq; right = $3 } }
    | expr Leq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Leq; right = $3 } }
    | expr Carot expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Carot; right = $3 } }
    | expr Ampersand expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Ampersand; right = $3 } }
    | expr Pipe expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Pipe; right = $3 } }
    | expr Xor expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Xor; right = $3 } }
    | expr Leftshift expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Leftshift; right = $3 } }
    | expr Rightshift expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Rightshift; right = $3 } }
    | New Identifier LParen RParen { Ast.Expr.NewExpr { class_name = $2 } }
    | Identifier LParen argument_list RParen { Ast.Expr.CallExpr { callee = Ast.Expr.VarExpr $1; arguments = $3 } }
    | expr Dot Identifier LParen argument_list RParen { Ast.Expr.MethodCall { obj = $1; method_name = $3; arguments = $5 } }
    | Not expr { Ast.Expr.UnaryExpr { operator = Ast.Not; operand = $2 } }
    | expr Inc { Ast.Expr.UnaryExpr { operator = Ast.Inc; operand = $1 } }
    | expr Dec { Ast.Expr.UnaryExpr { operator = Ast.Dec; operand = $1 } }
    | Minus IntLit { Ast.Expr.IntExpr { value = -$2 } }
    | Minus FloatLit { Ast.Expr.FloatExpr { value = -. $2 } }
    | Minus Identifier { Ast.Expr.UnaryExpr { operator = Ast.Minus; operand = Ast.Expr.VarExpr $2 } }
    | IntLit { Ast.Expr.IntExpr { value = $1 } }
    | FloatLit { Ast.Expr.FloatExpr { value = $1 } }
    | StringLit { Ast.Expr.StringExpr { value = $1 } }
    | CharLit { Ast.Expr.CharExpr { value = $1 } }
    | BoolLit { Ast.Expr.BoolExpr { value = $1 } }
    | Identifier { Ast.Expr.VarExpr $1 }
    | LParen expr RParen { $2 }
    | Alloc LParen expr RParen { Ast.Expr.AllocExpr { size = $3 } }
    | Dealloc LParen expr RParen { Ast.Expr.DeallocExpr { pointer = $3 } }
    | Sizeof LParen type_expr RParen { Ast.Expr.SizeofExpr { type_expr = $3 } }
    | Cast LParen expr Comma type_expr RParen { Ast.Expr.CastExpr { expr = $3; target_type = $5 } }
    | Typeof LParen expr RParen { Ast.Expr.TypeofExpr { expr = $3 } }
    | Println LParen expr RParen { Ast.Expr.PrintlnExpr { expr = $3 } }
    | Println LParen StringLit Comma argument_list RParen { Ast.Expr.PrintlnFormatExpr { format_string = $3; arguments = $5 } }
    | Length LParen expr RParen { Ast.Expr.LengthExpr { expr = $3 } }
    | Input LParen StringLit Comma type_expr RParen { Ast.Expr.InputExpr { prompt = $3; target_type = $5 } }
    | Identifier Assign expr { Ast.Expr.AssignmentExpr { identifier = $1; value = Some $3 } }
    | LBrace RBrace { Ast.Expr.ArrayExpr { elements = [] } }
    | LBrace expr_list RBrace { Ast.Expr.ArrayExpr { elements = $2 } }
    | Identifier LBracket expr RBracket { Ast.Expr.IndexExpr { array = Ast.Expr.VarExpr $1; index = $3 } }
    | Identifier Dot Identifier Assign expr { Ast.Expr.StructFieldAssign { struct_name = $1; field_name = $3; value = $5 } }
    | expr PlusAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.PlusAssign; right = $3 } }
    | expr MinusAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.MinusAssign; right = $3 } }
    | expr StarAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.StarAssign; right = $3 } }
    | expr SlashAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.SlashAssign; right = $3 } }
    | Identifier Dot Identifier { Ast.Expr.FieldAccess { object_name = $1; member_name = $3 } }
    | LParen expr RParen Question expr Colon expr { Ast.Expr.TernaryExpr { cond = $2; onTrue = $5; onFalse = $7; } }

parameter:
    | type_expr Identifier { Ast.Stmt.{ name = $2; param_type = $1 } }

parameter_list:
    | parameter Comma parameter_list { $1 :: $3 }
    | parameter { [$1] }
    | { [] }

case_list:
    | Case expr Colon stmt_list break_opt case_list { ($2, $4) :: $6 }
    | Case expr Colon stmt_list break_opt { [($2, $4)] }

default_opt:
    | Default Colon stmt_list { Some $3 }
    | { None }

break_opt:
    | Break Semi { Some Ast.Stmt.BreakStmt }
    | { None }

simple_stmt_opt:
    | simple_stmt { Some $1 }
    | { None }

expr_opt:
    | expr { Some $1 }
    | { None }

enum_member_list:
    | Identifier Comma enum_member_list { $1 :: $3 }
    | Identifier { [$1] }
    | { [] }

argument_list:
    | expr Comma argument_list { $1 :: $3 }
    | expr { [$1] }
    | { [] }

expr_list:
    | expr Comma expr_list { $1 :: $3 }
    | expr { [$1] }
    | { [] }

var_decl:
    | type_expr Identifier { ($2, $1) }

var_decl_list:
    | { [] }
    | var_decl var_decl_list { $1 :: $2 }

VarDeclStmt:
    | type_expr Star Identifier Assign expr { Ast.Stmt.VarDeclarationStmt { identifier = $3; constant = false; assigned_value = Some $5; explicit_type = Ast.Type.PointerType { base_type = $1 }; } }
    | type_expr Identifier Assign expr { Ast.Stmt.VarDeclarationStmt { identifier = $2; constant = false; assigned_value = Some $4; explicit_type = $1; } }
    | Const type_expr Identifier Assign expr { Ast.Stmt.VarDeclarationStmt { identifier = $3; constant = true; assigned_value = Some $5; explicit_type = $2; } }
    | type_expr Identifier { Ast.Stmt.VarDeclarationStmt { identifier = $2; constant = false; assigned_value = None; explicit_type = $1; } }

FuncDeclStmt:
    | Fn Identifier LParen parameter_list RParen type_expr LBrace stmt_list RBrace { Ast.Stmt.FuncDeclStmt { name = $2; parameters = $4; return_type = Some $6; body = $8; } }

ReturnStmt:
    | Return expr { Ast.Stmt.ReturnStmt $2 }

IfStmt:
    | If LParen expr RParen LBrace stmt_list RBrace Else LBrace stmt_list RBrace { Ast.Stmt.IfStmt { condition = $3; then_branch = Ast.Stmt.BlockStmt { body = $6 }; else_branch = Some (Ast.Stmt.BlockStmt { body = $10 }); } }
    | If LParen expr RParen LBrace stmt_list RBrace { Ast.Stmt.IfStmt { condition = $3; then_branch = Ast.Stmt.BlockStmt { body = $6 }; else_branch = None; } }

SwitchStmt:
    | Switch LParen expr RParen LBrace case_list default_opt RBrace { Ast.Stmt.SwitchStmt { expr = $3; cases = $6; default_case = $7 } }

WhileStmt:
    | While LParen expr RParen LBrace stmt_list RBrace { Ast.Stmt.WhileStmt { expr = $3; body = $6 } }

ForStmt:
    | For LParen simple_stmt_opt Semi expr_opt Semi simple_stmt_opt RParen LBrace stmt_list RBrace { Ast.Stmt.ForStmt { initialization = $3; condition = (match $5 with Some cond -> cond | None -> Ast.Expr.BoolExpr { value = true }); iteration = $7; body = Ast.Stmt.BlockStmt { body = $10 }; } }

EnumStmt:
    | Enum Identifier LBrace enum_member_list RBrace { Ast.Stmt.EnumDeclStmt { name = $2; members = $4; } }

UnsafeStmt:
    | Unsafe LBrace stmt_list RBrace { Ast.Stmt.UnsafeStmt { body = $3 } }

ImportStmt:
    | Import StringLit { Ast.Stmt.ImportStmt { module_name = $2 } }

ExportStmt:
    | Export Identifier { Ast.Stmt.ExportStmt { identifier = $2 } }

StructStmt:
    | Struct LBrace var_decl_list RBrace Identifier { Ast.Stmt.StructStmt { name = $5; fields = ($5, $3); priv = false } }
    | Struct Identifier LBrace var_decl_list RBrace { Ast.Stmt.StructStmt { name = $2; fields = ($2, $4); priv = false } }
    | Private Struct LBrace var_decl_list RBrace Identifier { Ast.Stmt.StructStmt { name = $6; fields = ($6, $4); priv = true } }
    | Private Struct Identifier LBrace var_decl_list RBrace { Ast.Stmt.StructStmt { name = $3; fields = ($3, $5); priv = true } }