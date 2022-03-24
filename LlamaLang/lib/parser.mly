%{ open Ast %}

/* Arithmetic operators  */
%token PLUS MINUS TIMES DIVIDE MODULO EXPON FLOOR INCREMENT DECREMENT
/* Assignment operators  */
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODULOASSIGN FLOORASSIGN EXPONASSIGN 
/* Logical    operators  */
%token LAND LOR LNOT
/* Comparison operators  */
%token EQ NEQ GT LT GEQ LEQ
/* Keywords logic        */
%token AND OR NOT
/* Keywords non-access   */
%token CONST FINAL
/* Keywords identity     */
%token IS ISNOT
/* Keywords membership   */
%token IN NOTIN
/* Keywords flow control */
%token WHEN WHILE IF ELSE BREAK CONTINUE DO FOR THEN SWITCH CASE DEFAULT
/* Keywords Object       */
%token CLASS CONSTRUCTOR NEW SUPER EXTENDS IMPLEMENTS DOT INTERFACE THROWS RAISES THIS
/* Keywords types        */
%token BOOL FLOAT CHAR STRING INT NULL
/* Keywords boolean lit  */
%token TRUE FALSE
/* Keywords imports      */
%token IMPORT AS
/* Keywords functions    */
%token RETURN VOID
/* Keywords exceptions   */
%token TRY CATCH FINALLY THROW RAISE
/* Delimiter characters  */
%token SEMICOLON COLON LCOMMENT RCOMMENT LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SINGLEQUOTE DOUBLEQUOTE BACKTICK
/* Scoping DELIMITERS    */
%token NEWLINE INDENT DEDENT 
/* Token                 */
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT 
%token <string> ID
%token <string> STRINGLIT
%token <char> CHARLIT
%token EOF

%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODULOASSIGN FLOORASSIGN EXPONASSIGN 
%left OR
%left AND
%nonassoc NOT
%left LOR
%left LAND
%nonassoc LNOT
%left EQ NEQ 
%left GT LT GEQ LEQ
%left PLUS MINUS 
%left TIMES DIVIDE MODULO FLOOR
%left EXPON
%nonassoc INCREMENT DECREMENT

%start holder
%type <Ast.holder> holder

%%

holder:
    PLUS {Var }
  | MINUS {Var }
  | TIMES {Var }
  | DIVIDE {Var }
  | MODULO {Var }
  | EXPON {Var }
  | FLOOR {Var }
  | INCREMENT {Var }
  | DECREMENT {Var }
  | ASSIGN {Var }
  | PLUSASSIGN {Var }
  | MINUSASSIGN {Var }
  | TIMESASSIGN {Var }
  | DIVIDEASSIGN {Var }
  | MODULOASSIGN {Var }
  | FLOORASSIGN {Var }
  | EXPONASSIGN {Var }
  | LAND {Var }
  | LOR {Var }
  | LNOT {Var }
  | EQ {Var }
  | NEQ {Var }
  | GT {Var }
  | LT {Var }
  | GEQ {Var }
  | LEQ {Var }
  | AND {Var }
  | OR {Var }
  | NOT {Var }
  | CONST {Var }
  | FINAL {Var }
  | IS {Var }
  | ISNOT {Var }
  | IN {Var }
  | NOTIN {Var }
  | WHEN {Var }
  | WHILE {Var }
  | IF {Var }
  | ELSE {Var }
  | BREAK {Var }
  | CONTINUE {Var }
  | DO {Var }
  | FOR {Var }
  | THEN {Var }
  | SWITCH {Var }
  | CASE {Var }
  | DEFAULT {Var }
  | CLASS {Var }
  | CONSTRUCTOR {Var }
  | NEW {Var }
  | SUPER {Var }
  | EXTENDS {Var }
  | IMPLEMENTS {Var }
  | DOT {Var }
  | INTERFACE {Var }
  | THROWS {Var }
  | RAISES {Var }
  | THIS {Var }
  | BOOL {Var }
  | FLOAT {Var }
  | CHAR {Var }
  | STRING {Var }
  | INT {Var }
  | NULL {Var }
  | TRUE {Var }
  | FALSE {Var }
  | IMPORT {Var }
  | AS {Var }
  | RETURN {Var }
  | VOID {Var }
  | TRY {Var }
  | CATCH {Var }
  | FINALLY {Var }
  | THROW {Var }
  | RAISE {Var }
  | SEMICOLON {Var }
  | COLON {Var }
  | LCOMMENT {Var }
  | RCOMMENT {Var }
  | LPAREN {Var }
  | RPAREN {Var }
  | LBRACKET {Var }
  | RBRACKET {Var }
  | LBRACE {Var }
  | RBRACE {Var }
  | COMMA {Var }
  | SINGLEQUOTE {Var }
  | DOUBLEQUOTE {Var }
  | BACKTICK {Var }
  | NEWLINE {Var }
  | INDENT {Var }
  | DEDENT {Var }
  | INTLIT {Var }
  | FLOATLIT {Var }
  | BOOLLIT  {Var }
  | ID {Var }
  | STRINGLIT {Var }
  | CHARLIT {Var }
  | EOF {Var }