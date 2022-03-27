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
%token CONST

/* Keywords identity     */
%token IS ISNOT

/* Keywords membership   */
%token IN NOTIN

/* Keywords flow control */
%token WHEN WHILE IF ELSE BREAK CONTINUE DO FOR THEN DEFAULT

/* Keywords Object       */
%token CLASS CONSTRUCTOR NEW SUPER EXTENDS IMPLEMENTS DOT INTERFACE THROWS THIS

/* Keywords types        */
%token BOOL FLOAT CHAR STRING INT NULL

/* Keywords boolean lit  */
%token TRUE FALSE

/* Keywords imports      */
%token IMPORT AS

/* Keywords functions    */
%token RETURN VOID

/* Keywords exceptions   */
%token TRY CATCH FINALLY THROW 

/* Delimiter characters  */
%token SEMICOLON COLON LCOMMENT RCOMMENT LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SINGLEQUOTE DOUBLEQUOTE BACKTICK

/* Scoping DELIMITERS    */
%token NEWLINE INDENT DEDENT 

/* Terminal Tokens       */
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

%start program
%type <Ast.program> program
%type <Ast.holder> holder


%%
program:
  statements EOF { $1 }

statements:
    /* nothing */         { []       }
  | statement statements  { $1::$2 }

statement:
    compound_statement { $1 }
  | simple_statements   { $1 }

simple_statements:
   /*nothing*/ { [] }
  | simple_statement NEWLINE { $1 }
  | simple_statement SEMICOLON simple_statements {  $1 :: $3 }

simple_statement:
    return_statement { $1 }
  | import_statement { $1 }
  | expression_statement { $1 }
  | throw_statement { $1 }

compound_statement:
    func { $1 }
  | if_statement { $1 }
  | for_statement { $1 }
  | when_statement { $1 }
  | while_statement { $1 }
  | try_statement { $1 }

return_statement:
  RETURN expr { Return($2) }

throw_statement:
  THROW expr { Throw($2) }

expression_statement:
  expr { Expr($1) }

import_statement:
    IMPORT expr { Import($2) }
  | IMPORT expr AS expr { Assign($2, Import($4)) }

if_statement:
      if_clause { If_stmt($1, [])}
    | if_clause if_body { If_stmt($1, $2)}

if_clause: 
    IF conditional COLON block { $2, $4 }

if_body:
   else_if_clauses else_clause { $1::[$2]}

else_if_clauses:
    /* nothing */ { [] }
  | else_if_clause else_if_clauses { $1::$2 }

else_if_clause:
    ELSE IF conditional COLON block { $3, $5 }

else_clause:
    ELSE COLON block { $3 }

for_statement:
    FOR id_decl IN expr COLON block { For_in( fst $2, snd $2, $4, $6 ) }
  | FOR args COLON block { For($2, $4) }

while_statement:
    WHILE expr COLON block { While($2, $4) }
 
when_statement:
    WHEN expr IS COLON NEWLINE when_body DEDENT { When($2, $6) }

when_body:
    case_statement case_statements default_statement { $1::$2::[$3] }

case_statements:
      /* nothing */ { [] } 
    | case_statement case_statements { $1::$2 }

case_statement:
    INDENT expr COLON NEWLINE INDENT block DEDENT { Case($2, $6) }

default_statement:
    INDENT DEFAULT COLON NEWLINE INDENT block DEDENT { Default($6) }

try_statement:
    TRY COLON block try_body { Try($3, $4, []) }
    | TRY COLON block try_body finally_block { Try($3, $4, $5) }

try_body:
    catch_block catch_blocks { $1::$2 }

catch_blocks:
      /* nothing */ { [] } 
    | catch_block catch_blocks { $1::$2 }

catch_block:
    CATCH expr COLON block { $2, $4 }

finally_block:
    FINALLY COLON block { $3 }

func:
  id_decl LPAREN params_opt RPAREN COLON block { Func(fst $1, snd $1, $3, $6) }

params_opt:
  /*nothing*/ { [] }
  | params_list { $1 }

params_list:
    id_decl { [$1] }
  | id_decl COMMA params_list { $1::$3 }

id_decl:
  typ expr { ($1, $2) }

typ:
    INT    { Int    }
  | FLOAT  { Float  }
  | BOOL   { Bool   }
  | CHAR   { Char   }
  | STRING { String } 
  | VOID   { Void   }

block:
    NEWLINE INDENT statements DEDENT { Block($3) }
  | simple_statements { Simple_stmts($1) }

conditional:
      LPAREN expr RPAREN { $2 }
    | expr { $1 }

expr:
    INTLIT     { IntLit($1)    }
  | FLOATLIT   { FloatLit($1)  }
  | BOOLLIT    { BoolLit($1)   }
  | CHARLIT    { CharLit($1)   }
  | STRINGLIT  { StringLit($1) }
  | ID         { Id($1)        }
  | TRUE       { BoolLit(true) }
  | FALSE      { BoolLit(false)}
  | expr PLUS   expr     { Binop($1, Add,   $3)     }
  | expr MINUS  expr     { Binop($1, Sub,   $3)     }
  | expr EQ     expr     { Binop($1, Eq,    $3)     }
  | expr NEQ    expr     { Binop($1, Neq,   $3)     }
  | expr LT     expr     { Binop($1, Lt,  $3)       }
  | expr AND    expr     { Binop($1, And,   $3)     }
  | expr OR     expr     { Binop($1, Or,    $3)     }
  | expr PLUSASSIGN expr {Assign($1, Binop($1, Add, $3)) }
  | expr MINUSASSIGN expr {Assign($1, Binop($1, Sub, $3)) }
  | expr TIMESASSIGN expr {Assign($1, Binop($1, Mult, $3)) }
  | expr DIVIDEASSIGN expr {Assign($1, Binop($1, Div, $3)) }
  | expr MODULOASSIGN expr {Assign($1, Binop($1, Mod, $3)) }
  | expr FLOORASSIGN expr {Assign($1, Binop($1, Floor, $3)) }
  | expr EXPONASSIGN expr {Assign($1, Binop($1, Exp, $3)) }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }

holder:
   ISNOT {Var }
  |CONSTRUCTOR {Var}
  |CLASS {Var}
  |SUPER {Var }
  | NEW {Var }
  | EXTENDS {Var }
  | IMPLEMENTS {Var }
  | INTERFACE {Var }
  | NOTIN {Var }
  | DOT {Var }
  | NULL {Var }