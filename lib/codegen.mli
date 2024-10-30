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
val struct_type : (string, Llvm.lltype * (string * Ast.Type.t) list) Hashtbl.t
val printf_type : Llvm.lltype
val printf_func : Llvm.llvalue
val malloc_type : Llvm.lltype
val malloc_func : Llvm.llvalue
val scanf_type : Llvm.lltype
val scanf_func : Llvm.llvalue
val find_field_index : (string * 'a) list -> string -> int
val get_struct_type : string -> Llvm.lltype * (string * Ast.Type.t) list
val define_struct_type : string -> (string * Ast.Type.t) list -> Llvm.lltype
val get_struct_instance : string -> Llvm.llvalue
val free_func : Llvm.llvalue
val strcpy_func : Llvm.llvalue
val strcat_func : Llvm.llvalue
val strlen_func : Llvm.llvalue
val concatenate_strings : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
val declare_malloc_function : 'a -> Llvm.llmodule -> Llvm.llvalue
val type_size_in_bytes : Ast.Type.t -> int
val string_of_llvm_type : Llvm.lltype -> string
val print_any_type : Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue
val is_char_type : Llvm.lltype -> bool
val codegen_expr : Ast.Expr.t -> Llvm.llvalue
val codegen_stmt : Ast.Stmt.t -> Llvm.llvalue
val generate_code : Ast.Stmt.t -> unit
