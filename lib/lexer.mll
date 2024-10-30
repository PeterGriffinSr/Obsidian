{
  open Parser

  let line = ref 1
  let column = ref 0
  
  let get_line () = !line
  let get_column () = !column
  
  let update_column () = incr column
  let update_line () = (incr line; column := 0)
  
  let update_column_with_lexeme lexbuf =
    let lexeme = Lexing.lexeme lexbuf in
    column := !column + String.length lexeme;
    lexeme
  
  let token_and_update_column t lexbuf =
    let token_length = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
    column := !column + token_length;
    t
}

let Identifier = ['a'-'z' 'A'-'Z']*
let Digits = ['0'-'9']+
let Floats = Digits '.' Digits+

rule token = parse
    | [' ' '\t']        { update_column (); token lexbuf }
    | '\n'              { update_line (); token lexbuf }
    | "//"              { read_comment lexbuf }

    | "int"                 { ignore (update_column_with_lexeme lexbuf); Int }
    | "float"               { ignore (update_column_with_lexeme lexbuf); Float }
    | "string"              { ignore (update_column_with_lexeme lexbuf); String }
    | "char"                { ignore (update_column_with_lexeme lexbuf); Char }
    | "bool"                { ignore (update_column_with_lexeme lexbuf); Bool }
    | "void"                { ignore (update_column_with_lexeme lexbuf); Void }

    | "const"               { ignore (update_column_with_lexeme lexbuf); Const }
    | "fn"                  { ignore (update_column_with_lexeme lexbuf); Fn }
    | "if"                  { ignore (update_column_with_lexeme lexbuf); If }
    | "else"                { ignore (update_column_with_lexeme lexbuf); Else }
    | "switch"              { ignore (update_column_with_lexeme lexbuf); Switch }
    | "case"                { ignore (update_column_with_lexeme lexbuf); Case }
    | "default"             { ignore (update_column_with_lexeme lexbuf); Default }
    | "while"               { ignore (update_column_with_lexeme lexbuf); While }
    | "for"                 { ignore (update_column_with_lexeme lexbuf); For }
    | "return"              { ignore (update_column_with_lexeme lexbuf); Return }
    | "struct"              { ignore (update_column_with_lexeme lexbuf); Struct }
    | "enum"                { ignore (update_column_with_lexeme lexbuf); Enum }
    | "new"                 { ignore (update_column_with_lexeme lexbuf); New }
    | "null"                { ignore (update_column_with_lexeme lexbuf); Null }
    | "true"                { ignore (update_column_with_lexeme lexbuf); BoolLit true }
    | "false"               { ignore (update_column_with_lexeme lexbuf); BoolLit false }
    | "alloc"               { ignore (update_column_with_lexeme lexbuf); Alloc }
    | "dealloc"             { ignore (update_column_with_lexeme lexbuf); Dealloc }
    | "sizeof"              { ignore (update_column_with_lexeme lexbuf); Sizeof }
    | "unsafe"              { ignore (update_column_with_lexeme lexbuf); Unsafe }
    | "private"             { ignore (update_column_with_lexeme lexbuf); Private }
    | "typeof"              { ignore (update_column_with_lexeme lexbuf); Typeof }
    | "import"              { ignore (update_column_with_lexeme lexbuf); Import }
    | "export"              { ignore (update_column_with_lexeme lexbuf); Export }
    | "cast"                { ignore (update_column_with_lexeme lexbuf); Cast }
    | "println"             { ignore (update_column_with_lexeme lexbuf); Println }
    | "input"               { ignore (update_column_with_lexeme lexbuf); Input }
    | "len"                 { ignore (update_column_with_lexeme lexbuf); Length }
    | "break"               { ignore (update_column_with_lexeme lexbuf); Break }

    | "("                   { token_and_update_column LParen lexbuf }
    | ")"                   { token_and_update_column RParen lexbuf }
    | "["                   { token_and_update_column LBracket lexbuf }
    | "]"                   { token_and_update_column RBracket lexbuf }
    | "{"                   { token_and_update_column LBrace lexbuf }
    | "}"                   { token_and_update_column RBrace lexbuf }
    | "+"                   { token_and_update_column Plus lexbuf }
    | "-"                   { token_and_update_column Minus lexbuf }
    | "*"                   { token_and_update_column Star lexbuf }
    | "/"                   { token_and_update_column Slash lexbuf }
    | "."                   { token_and_update_column Dot lexbuf }
    | ":"                   { token_and_update_column Colon lexbuf }
    | ";"                   { token_and_update_column Semi lexbuf }
    | ","                   { token_and_update_column Comma lexbuf }
    | "!"                   { token_and_update_column Not lexbuf }
    | ">"                   { token_and_update_column Greater lexbuf }
    | "<"                   { token_and_update_column Less lexbuf }
    | "^"                   { token_and_update_column Carot lexbuf }
    | "%"                   { token_and_update_column Percent lexbuf }
    | "="                   { token_and_update_column Assign lexbuf }
    | "&"                   { token_and_update_column Ampersand lexbuf }
    | "|"                   {token_and_update_column Pipe lexbuf }
    | "?"                   { token_and_update_column Question lexbuf }

    | "**"                  { column := !column + 2; Power }
    | "||"                  { column := !column + 2; LogicalOr }
    | "&&"                  { column := !column + 2; LogicalAnd }
    | "+="                  { column := !column + 2; PlusAssign }
    | "-="                  { column := !column + 2; MinusAssign }
    | "*="                  { column := !column + 2; StarAssign }
    | "/="                  { column := !column + 2; SlashAssign }
    | "=="                  { column := !column + 2; Eq }
    | "!="                  { column := !column + 2; Neq }
    | ">="                  { column := !column + 2; Geq }
    | "<="                  { column := !column + 2; Leq }
    | "--"                  { column := !column + 2; Dec }
    | "++"                  { column := !column + 2; Inc }
    | "^^"                  { column := !column + 2; Xor }
    | "<<"                  { column := !column + 2; Leftshift }
    | ">>"                  { column := !column + 2; Rightshift }

    | Identifier            { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; Identifier lexeme }
    | Floats                { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; FloatLit (float_of_string lexeme) }
    | Digits                { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; IntLit (int_of_string lexeme) }
    | '\'' [^'\''] '\''     { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; CharLit (lexeme.[1]) }
    | '"' [^'"']* '"'       { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; StringLit (String.sub lexeme 1 (String.length lexeme - 2)) }
    | eof                   { EOF }
    | _                     { let saved_line = get_line () in let saved_column = get_column () in let lexeme = Lexing.lexeme lexbuf in Printf.eprintf "Unexpected token '%s' at Line %d, Column %d\n" lexeme saved_line saved_column; exit (-1) }

and read_comment = parse
    | '\n'                 { incr line; column := 0; token lexbuf }
    | _                    { read_comment lexbuf }
    | eof                  { EOF }
