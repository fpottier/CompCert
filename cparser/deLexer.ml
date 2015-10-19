(* [identify] converts a terminal symbol (represented as a symbolic string) to a
   token, which carries a dummy semantic value. It may be used, after an error
   has been detected, to test whether the parser would accept a certain
   token. *)

(* [delex] does (almost) the opposite job and converts a token to a concrete
   string, which the lexer would accept. It may be used to convert an error
   sentence produced by Menhir to a concrete C file. *)

(* [identify] and [delex] should be maintained in sync with each other and with
   the lexer! *)

open Pre_parser
open Pre_parser_aux

let loc = Cabshelper.cabslu (* a dummy location *)

let identify (s : string) : token =
  match s with
  | "ALIGNAS" -> ALIGNAS loc
  | "ALIGNOF" -> ALIGNOF loc
  | "UNDERSCORE_BOOL" -> UNDERSCORE_BOOL loc
  | "ASM" -> ASM loc
  | "ATTRIBUTE" -> ATTRIBUTE loc
  | "BUILTIN_VA_ARG" -> BUILTIN_VA_ARG loc
  | "CONST" -> CONST loc
  | "INLINE" -> INLINE loc
  | "PACKED" -> PACKED loc
  | "RESTRICT" -> RESTRICT loc
  | "SIGNED" -> SIGNED loc
  | "VOLATILE" -> VOLATILE loc
  | "AUTO" -> AUTO loc
  | "BREAK" -> BREAK loc
  | "CASE" -> CASE loc
  | "CHAR" -> CHAR loc
  | "CONTINUE" -> CONTINUE loc
  | "DEFAULT" -> DEFAULT loc
  | "DO" -> DO loc
  | "DOUBLE" -> DOUBLE loc
  | "ELSE" -> ELSE loc
  | "ENUM" -> ENUM loc
  | "EXTERN" -> EXTERN loc
  | "FLOAT" -> FLOAT loc
  | "FOR" -> FOR loc
  | "GOTO" -> GOTO loc
  | "IF" -> IF loc
  | "INT" -> INT loc
  | "LONG" -> LONG loc
  | "REGISTER" -> REGISTER loc
  | "RETURN" -> RETURN loc
  | "SHORT" -> SHORT loc
  | "SIZEOF" -> SIZEOF loc
  | "STATIC" -> STATIC loc
  | "STRUCT" -> STRUCT loc
  | "SWITCH" -> SWITCH loc
  | "TYPEDEF" -> TYPEDEF loc
  | "UNION" -> UNION loc
  | "UNSIGNED" -> UNSIGNED loc
  | "VOID" -> VOID loc
  | "WHILE" -> WHILE loc
  | "TYPEDEF_NAME" -> TYPEDEF_NAME ("__builtin_va_list", ref TypedefId, loc)
  | "VAR_NAME" -> VAR_NAME ("x", ref VarId, loc)
  | "CONSTANT" -> CONSTANT (Cabs.CONST_INT "0", loc)
  | "STRING_LITERAL" -> STRING_LITERAL (false, [], loc)
  | "ELLIPSIS" -> ELLIPSIS loc
  | "ADD_ASSIGN" -> ADD_ASSIGN loc
  | "SUB_ASSIGN" -> SUB_ASSIGN loc
  | "MUL_ASSIGN" -> MUL_ASSIGN loc
  | "DIV_ASSIGN" -> DIV_ASSIGN loc
  | "MOD_ASSIGN" -> MOD_ASSIGN loc
  | "OR_ASSIGN"  -> OR_ASSIGN loc
  | "AND_ASSIGN" -> AND_ASSIGN loc
  | "XOR_ASSIGN" -> XOR_ASSIGN loc
  | "LEFT_ASSIGN" -> LEFT_ASSIGN loc
  | "RIGHT_ASSIGN" -> RIGHT_ASSIGN loc
  | "LEFT" -> LEFT loc
  | "RIGHT" -> RIGHT loc
  | "EQEQ" -> EQEQ loc
  | "NEQ" -> NEQ loc
  | "LEQ" -> LEQ loc
  | "GEQ" -> GEQ loc
  | "EQ" -> EQ loc
  | "LT" -> LT loc
  | "GT" -> GT loc
  | "INC" -> INC loc
  | "DEC" -> DEC loc
  | "PTR" -> PTR loc
  | "PLUS" -> PLUS loc
  | "MINUS" -> MINUS loc
  | "STAR" -> STAR loc
  | "SLASH" -> SLASH loc
  | "PERCENT" -> PERCENT loc
  | "BANG" -> BANG loc
  | "ANDAND" -> ANDAND loc
  | "BARBAR" -> BARBAR loc
  | "AND" -> AND loc
  | "BAR" -> BAR loc
  | "HAT" -> HAT loc
  | "QUESTION" -> QUESTION loc
  | "COLON" -> COLON loc
  | "TILDE" -> TILDE loc
  | "LBRACE" -> LBRACE loc
  | "RBRACE" -> RBRACE loc
  | "LBRACK" -> LBRACK loc
  | "RBRACK" -> RBRACK loc
  | "LPAREN" -> LPAREN loc
  | "RPAREN" -> RPAREN loc
  | "SEMICOLON" -> SEMICOLON loc
  | "COMMA" -> COMMA loc
  | "DOT" -> DOT loc
  | "PRAGMA" -> PRAGMA ("", loc)
  | "EOF" -> EOF
  | _ -> raise Not_found (* should not happen *)

let delex (token : token) : string =
  match token with
  | ALIGNAS loc -> "_Alignas"
  | ALIGNOF loc -> "_Alignof"
  | UNDERSCORE_BOOL loc -> "_Bool"
  | ASM loc -> "__asm"
  | ATTRIBUTE loc -> "__attribute"
  | BUILTIN_VA_ARG loc -> "__builtin_va_arg"
  | CONST loc -> "const"
  | INLINE loc -> "inline"
  | PACKED loc -> "__packed__"
  | RESTRICT loc -> "restrict"
  | SIGNED loc -> "signed"
  | VOLATILE loc -> "volatile"
  | AUTO loc -> "auto"
  | BREAK loc -> "break"
  | CASE loc -> "case"
  | CHAR loc -> "char"
  | CONTINUE loc -> "continue"
  | DEFAULT loc -> "default"
  | DO loc -> "do"
  | DOUBLE loc -> "double"
  | ELSE loc -> "else"
  | ENUM loc -> "enum"
  | EXTERN loc -> "extern"
  | FLOAT loc -> "float"
  | FOR loc -> "for"
  | GOTO loc -> "goto"
  | IF loc -> "if"
  | INT loc -> "int"
  | LONG loc -> "long"
  | REGISTER loc -> "register"
  | RETURN loc -> "return"
  | SHORT loc -> "short"
  | SIZEOF loc -> "sizeof"
  | STATIC loc -> "static"
  | STRUCT loc -> "struct"
  | SWITCH loc -> "switch"
  | TYPEDEF loc -> "typedef"
  | UNION loc -> "union"
  | UNSIGNED loc -> "unsigned"
  | VOID loc -> "void"
  | WHILE loc -> "while"
  | TYPEDEF_NAME (_, _, loc) -> "__builtin_va_list"
  | VAR_NAME (_, _, loc) -> "x" (* unless hidden by a type definition, "x" should be a variable *)
  | CONSTANT (_, loc) -> "42"
  | STRING_LITERAL (_, _, loc) -> "\"\""
  | ELLIPSIS loc -> "..."
  | ADD_ASSIGN loc -> "+="
  | SUB_ASSIGN loc -> "-="
  | MUL_ASSIGN loc -> "*="
  | DIV_ASSIGN loc -> "/="
  | MOD_ASSIGN loc -> "%="
  | OR_ASSIGN loc -> "|="
  | AND_ASSIGN loc -> "&="
  | XOR_ASSIGN loc -> "^="
  | LEFT_ASSIGN loc -> "<<="
  | RIGHT_ASSIGN loc -> ">>="
  | LEFT loc -> "<<"
  | RIGHT loc -> ">>"
  | EQEQ loc -> "=="
  | NEQ loc -> "!="
  | LEQ loc -> "<="
  | GEQ loc -> ">="
  | EQ loc -> "="
  | LT loc -> "<"
  | GT loc -> ">"
  | INC loc -> "++"
  | DEC loc -> "--"
  | PTR loc -> "->"
  | PLUS loc -> "+"
  | MINUS loc -> "-"
  | STAR loc -> "*"
  | SLASH loc -> "/"
  | PERCENT loc -> "%"
  | BANG loc -> "!"
  | ANDAND loc -> "&&"
  | BARBAR loc -> "||"
  | AND loc -> "&"
  | BAR loc -> "|"
  | HAT loc -> "^"
  | QUESTION loc -> "?"
  | COLON loc -> ":"
  | TILDE loc -> "~"
  | LBRACE loc -> "{"
  | RBRACE loc -> "}"
  | LBRACK loc -> "["
  | RBRACK loc -> "]"
  | LPAREN loc -> "("
  | RPAREN loc -> ")"
  | SEMICOLON loc -> ";"
  | COMMA loc -> ","
  | DOT loc -> "."
  | PRAGMA (_, loc) -> "#pragma \n"
  | EOF -> assert false (* cannot translate EOF to a string! *)

