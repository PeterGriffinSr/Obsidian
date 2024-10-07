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
val printf_type : Llvm.lltype
val printf_func : Llvm.llvalue
val malloc_type : Llvm.lltype
val malloc_func : Llvm.llvalue
val scanf_type : Llvm.lltype
val scanf_func : Llvm.llvalue
val strcpy_func : Llvm.llvalue
val strcat_func : Llvm.llvalue
val strlen_func : Llvm.llvalue
val free_func : Llvm.llvalue
val concatenate_strings : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
val type_size_in_bytes : Ast.Type.t -> int
val string_of_llvm_type : Llvm.lltype -> string
val print_any_type : Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue
val codegen_expr : Ast.Expr.t -> Llvm.llvalue
val codegen_stmt : Ast.Stmt.t -> Llvm.llvalue
val generate_code : Ast.Stmt.t -> unit
