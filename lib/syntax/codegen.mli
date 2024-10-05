val context : Llvm.llcontext
val the_module : Llvm.llmodule
val builder : Llvm.llbuilder
val i32_type : Llvm.lltype
val codegen_expr : Ast.Expr.t -> Llvm.llvalue
val codegen_stmt : Ast.Stmt.t -> Llvm.llvalue
val create_main_function : unit -> Llvm.llvalue
val generate_code : Ast.Stmt.t -> unit
