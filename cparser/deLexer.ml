(* [delex] converts a terminal symbol (represented as a symbolic string) to a
   concrete string, which the lexer would accept. *)

(* This can be used to convert an error sentence produced by Menhir to C code. *)

(* [delex] should be maintained in sync with the lexer! *)

let delex (symbol : string) : string =
  match symbol with
  | "ALIGNAS" -> "_Alignas"
  | "ALIGNOF" -> "_Alignof"
  | "UNDERSCORE_BOOL" -> "_Bool"
  | "ASM" -> "__asm"
  | "ATTRIBUTE" -> "__attribute"
  | "BUILTIN_VA_ARG" -> "__builtin_va_arg"
  | "CONST" -> "const"
  | "INLINE" -> "inline"
  | "PACKED" -> "__packed__"
  | "RESTRICT" -> "restrict"
  | "SIGNED" -> "signed"
  | "VOLATILE" -> "volatile"
  | "AUTO" -> "auto"
  | "BREAK" -> "break"
  | "CASE" -> "case"
  | "CHAR" -> "char"
  | "CONTINUE" -> "continue"
  | "DEFAULT" -> "default"
  | "DO" -> "do"
  | "DOUBLE" -> "double"
  | "ELSE" -> "else"
  | "ENUM" -> "enum"
  | "EXTERN" -> "extern"
  | "FLOAT" -> "float"
  | "FOR" -> "for"
  | "GOTO" -> "goto"
  | "IF" -> "if"
  | "INT" -> "int"
  | "LONG" -> "long"
  | "REGISTER" -> "register"
  | "RETURN" -> "return"
  | "SHORT" -> "short"
  | "SIZEOF" -> "sizeof"
  | "STATIC" -> "static"
  | "STRUCT" -> "struct"
  | "SWITCH" -> "switch"
  | "TYPEDEF" -> "typedef"
  | "UNION" -> "union"
  | "UNSIGNED" -> "unsigned"
  | "VOID" -> "void"
  | "WHILE" -> "while"
  | "TYPEDEF_NAME" -> "__builtin_va_list" (* this is a type name *)
  | "VAR_NAME" -> "x"          (* this should be a variable name *)
  | "CONSTANT" -> "42"
  | "STRING_LITERAL" -> "\"\""
  | "ELLIPSIS" -> "..."
  | "ADD_ASSIGN" -> "+="
  | "SUB_ASSIGN" -> "-="
  | "MUL_ASSIGN" -> "*="
  | "DIV_ASSIGN" -> "/="
  | "MOD_ASSIGN" -> "%="
  | "OR_ASSIGN" -> "|="
  | "AND_ASSIGN" -> "&="
  | "XOR_ASSIGN" -> "^="
  | "LEFT_ASSIGN" -> "<<="
  | "RIGHT_ASSIGN" -> ">>="
  | "LEFT" -> "<<"
  | "RIGHT" -> ">>"
  | "EQEQ" -> "=="
  | "NEQ" -> "!="
  | "LEQ" -> "<="
  | "GEQ" -> ">="
  | "EQ" -> "="
  | "LT" -> "<"
  | "GT" -> ">"
  | "INC" -> "++"
  | "DEC" -> "--"
  | "PTR" -> "->"
  | "PLUS" -> "+"
  | "MINUS" -> "-"
  | "STAR" -> "*"
  | "SLASH" -> "/"
  | "PERCENT" -> "%"
  | "BANG" -> "!"
  | "ANDAND" -> "&&"
  | "BARBAR" -> "||"
  | "AND" -> "&"
  | "BAR" -> "|"
  | "HAT" -> "^"
  | "QUESTION" -> "?"
  | "COLON" -> ":"
  | "TILDE" -> "~"
  | "LBRACE" -> "{"
  | "RBRACE" -> "}"
  | "LBRACK" -> "["
  | "RBRACK" -> "]"
  | "LPAREN" -> "("
  | "RPAREN" -> ")"
  | "SEMICOLON" -> ";"
  | "COMMA" -> ","
  | "DOT" -> "."
  | "PRAGMA" -> "#pragma \n"
  | "EOF" -> ""                             (* this should be ok *)
  | _ -> raise Not_found               (* this should not happen *)

(* De-lexing a sentence. *)

let delex sentence =
  let symbols = Str.split (Str.regexp " ") sentence in
  let symbols = List.map delex symbols in
  List.iter (fun symbol ->
    Printf.printf "%s " symbol
  ) symbols

(* This file is meant to be run as a script. We read one line from the standard
   input channel and delex it. *)

let () =
  delex (input_line stdin);
  print_newline()

