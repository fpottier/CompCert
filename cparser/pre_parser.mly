/* *********************************************************************/
/*                                                                     */
/*              The Compcert verified compiler                         */
/*                                                                     */
/*          Jacques-Henri Jourdan, INRIA Paris-Rocquencourt            */
/*                                                                     */
/*  Copyright Institut National de Recherche en Informatique et en     */
/*  Automatique.  All rights reserved.  This file is distributed       */
/*  under the terms of the GNU General Public License as published by  */
/*  the Free Software Foundation, either version 2 of the License, or  */
/*  (at your option) any later version.  This file is also distributed */
/*  under the terms of the INRIA Non-Commercial License Agreement.     */
/*                                                                     */
/* *********************************************************************/

%{
  open Pre_parser_aux

  let set_id_type (_,r,_) t =
    r := t

  let declare_varname (i,_,_) =
    !declare_varname i

  let declare_typename (i,_,_) =
    !declare_typename i

%}

%token<string * Pre_parser_aux.identifier_type ref * Cabs.cabsloc>
  VAR_NAME TYPEDEF_NAME
%token<Cabs.constant * Cabs.cabsloc> CONSTANT
%token<bool * int64 list * Cabs.cabsloc> STRING_LITERAL
%token<string * Cabs.cabsloc> PRAGMA

%token<Cabs.cabsloc> SIZEOF PTR INC DEC LEFT RIGHT LEQ GEQ EQEQ EQ NEQ LT GT
  ANDAND BARBAR PLUS MINUS STAR TILDE BANG SLASH PERCENT HAT BAR QUESTION
  COLON AND MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN LEFT_ASSIGN
  RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN LPAREN RPAREN LBRACK RBRACK
  LBRACE RBRACE DOT COMMA SEMICOLON ELLIPSIS TYPEDEF EXTERN STATIC RESTRICT
  AUTO REGISTER INLINE CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE
  UNDERSCORE_BOOL CONST VOLATILE VOID STRUCT UNION ENUM CASE DEFAULT IF ELSE
  SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN BUILTIN_VA_ARG ALIGNOF
  ATTRIBUTE ALIGNAS PACKED ASM

%token EOF

(* These precedences declarations solve the conflict in the following declaration :

int f(int (a));

when a is a TYPEDEF_NAME. It is specified by 6.7.5.3 11.
*)
%nonassoc TYPEDEF_NAME
%nonassoc highPrec

%start<unit> translation_unit_file
%%

(* Helpers *)

option(X):
| /* nothing */
    { None }
| x = X
    { Some x }

%inline ioption(X):
| /* nothing */
    { None }
| x = X
    { Some x }

%inline fst(X):
| x = X
    { fst x }

general_identifier:
| i = VAR_NAME
| i = TYPEDEF_NAME
    { i }

%inline other_identifier:
  i = general_identifier
    { set_id_type i OtherId }

string_literals_list:
| STRING_LITERAL
| string_literals_list STRING_LITERAL
    {}

(* WARNING : because of the lookahead token, the context might
   be pushed or popped one token after the position of this
   non-terminal !

   Pushing too late is not dangerous for us, because this does not
   change the token stream. However, we have to make sure the
   lookahead token present just after popping is not an identifier.
 *)

push_context:
  (* empty *)%prec highPrec { !push_context () }
pop_context:
  (* empty *) { !pop_context () }
in_context(nt):
  push_context x = nt pop_context { x }

declare_varname(nt):
  i = nt { declare_varname i; i }

declare_typename(nt):
  i = nt { declare_typename i; i }

(* Actual grammar *)

primary_expression(context):
| i = VAR_NAME
    { set_id_type i VarId }
| CONSTANT
| string_literals_list
| LPAREN expression(RPAREN) RPAREN
    {}

postfix_expression(context):
| primary_expression(context)
| postfix_expression(context) LBRACK expression(RBRACK) RBRACK
| postfix_expression(context) LPAREN argument_expression_list? RPAREN
    {}
| BUILTIN_VA_ARG LPAREN assignment_expression(COMMA) COMMA type_name RPAREN
    {}
| postfix_expression(context) DOT other_identifier
| postfix_expression(context) PTR other_identifier
    {}
| postfix_expression(context) INC
| postfix_expression(context) DEC
| LPAREN type_name RPAREN LBRACE initializer_list COMMA? RBRACE
    {}

argument_expression_list:
| assignment_expression(argument_expression_list)
| argument_expression_list COMMA assignment_expression(argument_expression_list)
    {}

unary_expression(context):
| postfix_expression(context)
| INC unary_expression(context)
| DEC unary_expression(context)
| unary_operator cast_expression(context)
| SIZEOF unary_expression(context)
| SIZEOF LPAREN type_name RPAREN
| ALIGNOF unary_expression(context)
| ALIGNOF LPAREN type_name RPAREN
    {}

unary_operator:
| AND
| STAR
| PLUS
| MINUS
| TILDE
| BANG
    {}

cast_expression(context):
| unary_expression(context)
| LPAREN type_name RPAREN cast_expression(context)
    {}

multiplicative_expression(context):
| cast_expression(context)
| multiplicative_expression(context) STAR cast_expression(context)
| multiplicative_expression(context) SLASH cast_expression(context)
| multiplicative_expression(context) PERCENT cast_expression(context)
    {}

additive_expression(context):
| multiplicative_expression(context)
| additive_expression(context) PLUS multiplicative_expression(context)
| additive_expression(context) MINUS multiplicative_expression(context)
    {}

shift_expression(context):
| additive_expression(context)
| shift_expression(context) LEFT additive_expression(context)
| shift_expression(context) RIGHT additive_expression(context)
    {}

relational_expression(context):
| shift_expression(context)
| relational_expression(context) LT shift_expression(context)
| relational_expression(context) GT shift_expression(context)
| relational_expression(context) LEQ shift_expression(context)
| relational_expression(context) GEQ shift_expression(context)
    {}

equality_expression(context):
| relational_expression(context)
| equality_expression(context) EQEQ relational_expression(context)
| equality_expression(context) NEQ relational_expression(context)
    {}

and_expression(context):
| equality_expression(context)
| and_expression(context) AND equality_expression(context)
    {}

exclusive_or_expression(context):
| and_expression(context)
| exclusive_or_expression(context) HAT and_expression(context)
    {}

inclusive_or_expression(context):
| exclusive_or_expression(context)
| inclusive_or_expression(context) BAR exclusive_or_expression(context)
    {}

logical_and_expression(context):
| inclusive_or_expression(context)
| logical_and_expression(context) ANDAND inclusive_or_expression(context)
    {}

logical_or_expression(context):
| logical_and_expression(context)
| logical_or_expression(context) BARBAR logical_and_expression(context)
    {}

conditional_expression(context):
| logical_or_expression(context)
| logical_or_expression(context) QUESTION expression(COLON) COLON conditional_expression(context)
    {}

assignment_expression(context):
| conditional_expression(context)
| unary_expression(context) assignment_operator assignment_expression(context)
    {}

assignment_operator:
| EQ
| MUL_ASSIGN
| DIV_ASSIGN
| MOD_ASSIGN
| ADD_ASSIGN
| SUB_ASSIGN
| LEFT_ASSIGN
| RIGHT_ASSIGN
| AND_ASSIGN
| XOR_ASSIGN
| OR_ASSIGN
    {}

expression(context):
| assignment_expression(context)
| expression(context) COMMA assignment_expression(context)
    {}

constant_expression(context):
| conditional_expression(context)
    {}

declaration:
| declaration_specifiers init_declarator_list? SEMICOLON
    {}
| declaration_specifiers_typedef typedef_declarator_list? SEMICOLON
    {}

declaration_specifiers_no_type:
| storage_class_specifier_no_typedef declaration_specifiers_no_type?
| type_qualifier                     declaration_specifiers_no_type?
| function_specifier                 declaration_specifiers_no_type?
    {}

declaration_specifiers_no_typedef_name:
| storage_class_specifier_no_typedef declaration_specifiers_no_typedef_name?
| type_qualifier                     declaration_specifiers_no_typedef_name?
| function_specifier                 declaration_specifiers_no_typedef_name?
| type_specifier_no_typedef_name     declaration_specifiers_no_typedef_name?
    {}

declaration_specifiers:
| ioption(declaration_specifiers_no_type) i = TYPEDEF_NAME declaration_specifiers_no_type?
    { set_id_type i TypedefId }
| ioption(declaration_specifiers_no_type) type_specifier_no_typedef_name declaration_specifiers_no_typedef_name?
    {}

declaration_specifiers_typedef:
| ioption(declaration_specifiers_no_type) TYPEDEF declaration_specifiers_no_type? i = TYPEDEF_NAME declaration_specifiers_no_type?
| ioption(declaration_specifiers_no_type) i = TYPEDEF_NAME declaration_specifiers_no_type? TYPEDEF declaration_specifiers_no_type?
    { set_id_type i TypedefId }
| ioption(declaration_specifiers_no_type) TYPEDEF declaration_specifiers_no_type? type_specifier_no_typedef_name declaration_specifiers_no_typedef_name?
| ioption(declaration_specifiers_no_type) type_specifier_no_typedef_name declaration_specifiers_no_typedef_name? TYPEDEF declaration_specifiers_no_typedef_name?
    {}

init_declarator_list:
| init_declarator
| init_declarator_list COMMA init_declarator
    {}

init_declarator:
| declare_varname(fst(declarator))
| declare_varname(fst(declarator)) EQ c_initializer
    { }

typedef_declarator_list:
| typedef_declarator
| typedef_declarator_list COMMA typedef_declarator
    {}

typedef_declarator:
| declare_typename(fst(declarator))
    { }

storage_class_specifier_no_typedef:
| EXTERN
| STATIC
| AUTO
| REGISTER
    {}

type_specifier_no_typedef_name:
| VOID
| CHAR
| SHORT
| INT
| LONG
| FLOAT
| DOUBLE
| SIGNED
| UNSIGNED
| UNDERSCORE_BOOL
| struct_or_union_specifier
| enum_specifier
    {}

struct_or_union_specifier:
| struct_or_union attribute_specifier_list other_identifier? LBRACE struct_declaration_list RBRACE
| struct_or_union attribute_specifier_list other_identifier
    {}

struct_or_union:
| STRUCT
| UNION
    {}

struct_declaration_list:
| (* empty *)
| struct_declaration_list struct_declaration
    {}

struct_declaration:
| specifier_qualifier_list(struct_declaration) struct_declarator_list? SEMICOLON
    {}

(* The [context] parameter is unused. It does not influence the language
   that is accepted. It records the identity of the caller (here, either
   [struct_declaration] or [type_name]). This forces a distinction between
   certain states in the automaton, and allows us to give more precise
   syntax error messages. -fpottier *)
specifier_qualifier_list(context):
| type_qualifier_list? i = TYPEDEF_NAME type_qualifier_list?
    { set_id_type i TypedefId }
| type_qualifier_list? type_specifier_no_typedef_name specifier_qualifier_list_no_typedef_name(context)?
    {}

(* The [context] parameter is unused. It does not influence the language
   that is accepted. It records the identity of the caller (here, either
   [struct_declaration] or [type_name]). This forces a distinction between
   certain states in the automaton, and allows us to give more precise
   syntax error messages. -fpottier *)
specifier_qualifier_list_no_typedef_name(context):
| type_specifier_no_typedef_name specifier_qualifier_list_no_typedef_name(context)?
| type_qualifier                 specifier_qualifier_list_no_typedef_name(context)?
    {}

struct_declarator_list:
| struct_declarator
| struct_declarator_list COMMA struct_declarator
    {}

struct_declarator:
| declarator
| declarator? COLON constant_expression(struct_declarator)
    {}

enum_specifier:
| ENUM attribute_specifier_list other_identifier? LBRACE enumerator_list COMMA? RBRACE
| ENUM attribute_specifier_list other_identifier
    {}

enumerator_list:
| declare_varname(enumerator)
| enumerator_list COMMA declare_varname(enumerator)
    {}

enumerator:
| i = enumeration_constant
| i = enumeration_constant EQ constant_expression(enumerator)
    { i }

enumeration_constant:
| i = general_identifier
    { set_id_type i VarId; i }

type_qualifier:
| CONST
| RESTRICT
| VOLATILE
| attribute_specifier
    {}

attribute_specifier_list:
| /* empty */
| attribute_specifier_list attribute_specifier
    {}

attribute_specifier:
| ATTRIBUTE LPAREN LPAREN gcc_attribute_list RPAREN RPAREN
| PACKED LPAREN argument_expression_list RPAREN
(* TODO: slove conflict *)
(* | PACKED *)
| ALIGNAS LPAREN argument_expression_list RPAREN
| ALIGNAS LPAREN type_name RPAREN
    {}

gcc_attribute_list:
| gcc_attribute
| gcc_attribute_list COMMA gcc_attribute
    {}

gcc_attribute:
| /* empty */
| gcc_attribute_word
| gcc_attribute_word LPAREN argument_expression_list? RPAREN
    {}
| gcc_attribute_word LPAREN i = TYPEDEF_NAME COMMA argument_expression_list RPAREN
    (* This is to emulate GCC's attribute syntax : we make this identifier
       a var name identifier, so that the parser will see it as a variable
       reference *)
    { set_id_type i VarId }

gcc_attribute_word:
| other_identifier
| CONST
| PACKED
    {}

function_specifier:
| INLINE
    {}

declarator:
| ioption(pointer) x = direct_declarator attribute_specifier_list
    { x }

direct_declarator:
| i = general_identifier
    { set_id_type i VarId; (i, None) }
| LPAREN x = declarator RPAREN
| x = direct_declarator LBRACK type_qualifier_list? ioption(assignment_expression(RBRACK)) RBRACK
    /* Using ioption above, even though option would work,
       because knowing whether the size has been read allows
       us to give better syntax error messages. -fpottier */
    { x }
| x = direct_declarator LPAREN l=in_context(parameter_type_list?) RPAREN
    { match snd x with
      | None -> (fst x, Some (match l with None -> [] | Some l -> l))
      | Some _ -> x }

pointer:
| STAR type_qualifier_list?
| STAR type_qualifier_list? pointer
    {}

type_qualifier_list:
| type_qualifier_list? type_qualifier
    {}

parameter_type_list:
| l=parameter_list
| l=parameter_list COMMA ELLIPSIS
    { l }

parameter_list:
| i=parameter_declaration
    { [i] }
| l=parameter_list COMMA i=parameter_declaration
    { i::l }

parameter_declaration:
| declaration_specifiers id=declare_varname(fst(declarator))
    { Some id }
| declaration_specifiers abstract_declarator(parameter_declaration)?
    { None }

type_name:
| specifier_qualifier_list(type_name) abstract_declarator(type_name)?
    {}

(* The [context] parameter is unused. It does not influence the language
   that is accepted. It records the identity of the caller (here, either
   [parameter_declaration] or [type_name]). This forces a distinction between
   certain states in the automaton, and allows us to give more precise
   syntax error messages. -fpottier *)
abstract_declarator(context):
| pointer
| ioption(pointer) direct_abstract_declarator
    {}

/* This is semantically equivalent to abstract_declarator, but is
   inlined at its use site. This improves the static context and
   allows us to give better syntax error messages. -fpottier */
%inline inline_abstract_declarator:
| pointer
| ioption(pointer) direct_abstract_declarator
    {}

direct_abstract_declarator:
| LPAREN inline_abstract_declarator RPAREN
| option(direct_abstract_declarator) LBRACK type_qualifier_list? ioption(assignment_expression(RBRACK)) RBRACK
    /* Using ioption above, even though option would work,
       because knowing whether the size has been read allows
       us to give better syntax error messages. -fpottier */
| ioption(direct_abstract_declarator) LPAREN in_context(parameter_type_list?) RPAREN
    {}

c_initializer:
| assignment_expression(c_initializer)
| LBRACE initializer_list COMMA? RBRACE
    {}

initializer_list:
| designation? c_initializer
| initializer_list COMMA designation? c_initializer
    {}

designation:
| designator_list EQ
    {}

designator_list:
| designator_list? designator
    {}

designator:
| LBRACK constant_expression(RBRACK) RBRACK
| DOT other_identifier
    {}

statement_finish:
| labeled_statement(statement_finish)
| compound_statement
| expression_statement
| selection_statement_finish
| iteration_statement(statement_finish)
| jump_statement
| asm_statement
    {}

statement_intern:
| labeled_statement(statement_intern)
| compound_statement
| expression_statement
| selection_statement_intern
| iteration_statement(statement_intern)
| jump_statement
| asm_statement
    {}

labeled_statement(last_statement):
| other_identifier COLON last_statement
| CASE constant_expression(COLON) COLON last_statement
| DEFAULT COLON last_statement
    {}

compound_statement:
| LBRACE in_context(block_item_list?) RBRACE
    {}

block_item_list:
| block_item_list? block_item
    {}

block_item:
| declaration
| statement_finish
| PRAGMA
    {}

expression_statement:
| expression(SEMICOLON)? SEMICOLON
    {}

selection_statement_finish:
| IF LPAREN expression(RPAREN) RPAREN statement_finish
| IF LPAREN expression(RPAREN) RPAREN statement_intern ELSE statement_finish
| SWITCH LPAREN expression(RPAREN) RPAREN statement_finish
    {}

selection_statement_intern:
| IF LPAREN expression(RPAREN) RPAREN statement_intern ELSE statement_intern
| SWITCH LPAREN expression(RPAREN) RPAREN statement_intern
    {}

iteration_statement(stmt):
| WHILE LPAREN expression(RPAREN) RPAREN stmt
| DO statement_finish WHILE LPAREN expression(RPAREN) RPAREN SEMICOLON
| FOR LPAREN expression(SEMICOLON)? SEMICOLON expression(SEMICOLON)? SEMICOLON expression(RPAREN)? RPAREN stmt
| FOR LPAREN push_context declaration expression(SEMICOLON)? SEMICOLON expression(RPAREN)? RPAREN stmt pop_context
    {}

jump_statement:
| GOTO other_identifier SEMICOLON
| CONTINUE SEMICOLON
| BREAK SEMICOLON
| RETURN expression(SEMICOLON)? SEMICOLON
    {}

asm_statement:
| ASM asm_attributes LPAREN string_literals_list asm_arguments RPAREN SEMICOLON
    {}

asm_attributes:
| /* empty */
| CONST asm_attributes    
| VOLATILE asm_attributes
    {}

asm_arguments:
| /* empty */
| COLON asm_operands
| COLON asm_operands COLON asm_operands
| COLON asm_operands COLON asm_operands COLON asm_flags
    {}

asm_operands:
| /* empty */
| asm_operands_ne
    {}

asm_operands_ne:
| asm_operands_ne COMMA asm_operand
| asm_operand
    {}

asm_operand:
| asm_op_name string_literals_list LPAREN expression(RPAREN) RPAREN
    {}

asm_op_name:
| /*empty*/
| LBRACK other_identifier RBRACK
    {}

asm_flags:
| string_literals_list
| string_literals_list COMMA asm_flags
    {}

translation_unit_file:
| translation_unit EOF
| EOF
    {}

translation_unit:
| external_declaration
| translation_unit external_declaration
| translation_unit SEMICOLON
| SEMICOLON
    {}

external_declaration:
| function_definition
| declaration
| PRAGMA
    {}

function_definition_begin:
| declaration_specifiers ioption(pointer) x=direct_declarator
    { match x with
      | (_, None) -> $syntaxerror
      | (i, Some l) ->
	declare_varname i;
	!push_context ();
	List.iter (fun x ->
	  match x with
	    | None -> ()
	    | Some i -> declare_varname i
	  ) l
    }
| declaration_specifiers ioption(pointer) x=direct_declarator 
  LPAREN params=identifier_list RPAREN in_context(declaration_list)
    { match x with
      | (_, Some _) -> $syntaxerror
      | (i, None) ->
	declare_varname i;
	!push_context ();
	List.iter declare_varname params
    }

identifier_list:
| id = VAR_NAME
    { [id] }
| idl = identifier_list COMMA id = VAR_NAME
    { id :: idl }

declaration_list:
| /*empty*/
    { }
| declaration_list declaration
    { }

function_definition:
| function_definition_begin LBRACE block_item_list? pop_context RBRACE
    { }

