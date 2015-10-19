(* The de-lexer converts a terminal symbol (represented as a string) to a token,
   which carries a dummy semantic value. It is used, after an error has been
   detected, to test whether the parser would accept a certain token. *)

(* The de-lexer should be maintained in sync with the lexer! In order to help,
   we write the function [name] below, which should contain the same cases
   (mirrored) as the de-lexer. OCaml will warn if this function is not
   exhaustive. *)

open Pre_parser
open Pre_parser_aux

let loc = Cabshelper.cabslu (* a dummy location *)

let delex (s : string) : token =
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

let name token =
  match token with
  | ALIGNAS loc -> "ALIGNAS"
  | ALIGNOF loc -> "ALIGNOF"
  | UNDERSCORE_BOOL loc -> "UNDERSCORE_BOOL"
  | ASM loc -> "ASM"
  | ATTRIBUTE loc -> "ATTRIBUTE"
  | BUILTIN_VA_ARG loc -> "BUILTIN_VA_ARG"
  | CONST loc -> "CONST"
  | INLINE loc -> "INLINE"
  | PACKED loc -> "PACKED"
  | RESTRICT loc -> "RESTRICT"
  | SIGNED loc -> "SIGNED"
  | VOLATILE loc -> "VOLATILE"
  | AUTO loc -> "AUTO"
  | BREAK loc -> "BREAK"
  | CASE loc -> "CASE"
  | CHAR loc -> "CHAR"
  | CONTINUE loc -> "CONTINUE"
  | DEFAULT loc -> "DEFAULT"
  | DO loc -> "DO"
  | DOUBLE loc -> "DOUBLE"
  | ELSE loc -> "ELSE"
  | ENUM loc -> "ENUM"
  | EXTERN loc -> "EXTERN"
  | FLOAT loc -> "FLOAT"
  | FOR loc -> "FOR"
  | GOTO loc -> "GOTO"
  | IF loc -> "IF"
  | INT loc -> "INT"
  | LONG loc -> "LONG"
  | REGISTER loc -> "REGISTER"
  | RETURN loc -> "RETURN"
  | SHORT loc -> "SHORT"
  | SIZEOF loc -> "SIZEOF"
  | STATIC loc -> "STATIC"
  | STRUCT loc -> "STRUCT"
  | SWITCH loc -> "SWITCH"
  | TYPEDEF loc -> "TYPEDEF"
  | UNION loc -> "UNION"
  | UNSIGNED loc -> "UNSIGNED"
  | VOID loc -> "VOID"
  | WHILE loc -> "WHILE"
  | TYPEDEF_NAME (_, _, loc) -> "TYPEDEF_NAME"
  | VAR_NAME (_, _, loc) -> "VAR_NAME"
  | CONSTANT (_, loc) -> "CONSTANT"
  | STRING_LITERAL (_, _, loc) -> "STRING_LITERAL"
  | ELLIPSIS loc -> "ELLIPSIS"
  | ADD_ASSIGN loc -> "ADD_ASSIGN"
  | SUB_ASSIGN loc -> "SUB_ASSIGN"
  | MUL_ASSIGN loc -> "MUL_ASSIGN"
  | DIV_ASSIGN loc -> "DIV_ASSIGN"
  | MOD_ASSIGN loc -> "MOD_ASSIGN"
  | OR_ASSIGN loc -> "OR_ASSIGN"
  | AND_ASSIGN loc -> "AND_ASSIGN"
  | XOR_ASSIGN loc -> "XOR_ASSIGN"
  | LEFT_ASSIGN loc -> "LEFT_ASSIGN"
  | RIGHT_ASSIGN loc -> "RIGHT_ASSIGN"
  | LEFT loc -> "LEFT"
  | RIGHT loc -> "RIGHT"
  | EQEQ loc -> "EQEQ"
  | NEQ loc -> "NEQ"
  | LEQ loc -> "LEQ"
  | GEQ loc -> "GEQ"
  | EQ loc -> "EQ"
  | LT loc -> "LT"
  | GT loc -> "GT"
  | INC loc -> "INC"
  | DEC loc -> "DEC"
  | PTR loc -> "PTR"
  | PLUS loc -> "PLUS"
  | MINUS loc -> "MINUS"
  | STAR loc -> "STAR"
  | SLASH loc -> "SLASH"
  | PERCENT loc -> "PERCENT"
  | BANG loc -> "BANG"
  | ANDAND loc -> "ANDAND"
  | BARBAR loc -> "BARBAR"
  | AND loc -> "AND"
  | BAR loc -> "BAR"
  | HAT loc -> "HAT"
  | QUESTION loc -> "QUESTION"
  | COLON loc -> "COLON"
  | TILDE loc -> "TILDE"
  | LBRACE loc -> "LBRACE"
  | RBRACE loc -> "RBRACE"
  | LBRACK loc -> "LBRACK"
  | RBRACK loc -> "RBRACK"
  | LPAREN loc -> "LPAREN"
  | RPAREN loc -> "RPAREN"
  | SEMICOLON loc -> "SEMICOLON"
  | COMMA loc -> "COMMA"
  | DOT loc -> "DOT"
  | PRAGMA (_, loc) -> "PRAGMA"
  | EOF -> "EOF"

