%{ open Ast }%

(* Arithmetic operators  *)
%token PLUS MINUS TIMES DIVIDE MODULO EXPON FLOOR INCREMENT DECREMENT
(* Assignment operators  *)
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODULOASSIGN FLOORASSIGN EXPONASSIGN 
(* Logical    operators  *)
%token LAND LOR LNOT
(* Comparison operators  *)
%token EQ NEQ GT LT GEQ LEQ
(* Keywords logic        *)
%token AND OR NOT
(* Keywords non-access   *)
%token CONST FINAL
(* Keywords identity     *)
%token IS ISNOT
(* Keywords membership   *)
%token IN NOTIN
(* Keywords flow control *)
%token WHEN WHILE IF ELSE BREAK CONTINUE DO FOR THEN SWITCH CASE DEFAULT
(* Keywords Object       *)
%token CLASS CONSTRUCTOR NEW SUPER EXTENDS IMPLEMENTS DOT INTERFACE THROWS RAISES THIS
(* Keywords types        *)
%token BOOL FLOAT CHAR STRING INT NULL
(* Keywords boolean lit  *)
%token TRUE FALSE
(* Keywords imports      *)
%token IMPORT AS
(* Keywords functions    *)
%token RETURN VOID
(* Keywords exceptions   *)
%token TRY CATCH FINALLY THROW RAISE
(* Delimiter characters  *)
%token SEMICOLON COLON LCOMMENT RCOMMENT LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SINGLEQUOTE DOUBLEQUOTE BACKTICK
(* Scoping DELIMITERS   *)
%token NEWLINE INDENT DEDENT 
(* Token                 *)
%token <int> INTLIT
%token <float>
%token <bool> BOOLLIT 
%token <string> ID
%token EOF