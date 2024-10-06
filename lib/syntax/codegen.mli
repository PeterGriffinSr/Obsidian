val context : Llvm.llcontext
val the_module : Llvm.llmodule
val builder : Llvm.llbuilder
val i64_type : Llvm.lltype
val f64_type : Llvm.lltype
val i1_type : Llvm.lltype
val char_type : Llvm.lltype
val void_type : Llvm.lltype
val string_type : Llvm.lltype
val variables : (string, Llvm.llvalue) Hashtbl.t
val codegen_expr : Ast.Expr.t -> Llvm.llvalue
val codegen_stmt : Ast.Stmt.t -> Llvm.llvalue
val generate_code : Ast.Stmt.t -> unit
