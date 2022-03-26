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

%%

program:
  stmts EOF {$1}

statements:
   statement*

statement:
     simple_stmts {$1}
   | compound_stmt {[$1]}

simple_stmts:
     simple_stmt NEWLINE {}
   | simple_stmt SEMICOLON simple_stmt+ NEWLINE

simple_stmt:
    dec_stmt {}
  | assign_stmt {}
  | return_stmt {}
  | import_stmt {}
  | raise_stmt  {}

compound_stmt:
     func_def {}
   | class_def {}
   | while_stmt {}
   | for_stmt {}
   | try_stmt {}
   | when_stmt {}

dec_stmt:
    


assign_stmt:
     ID ASSIGN expr { Assign()}
   | ID PLUSASSIGN expr {}
   | ID MINUSASSIGN expr {}
   | ID TIMESASSIGN expr {}
   | ID DIVIDEASSIGN expr {}
   | ID MODULOASSIGN expr {}
   | ID FLOORASSIGN expr {}
   | ID EXPONASSIGN expr {}

if_stmt:
   IF conditional COLON block
   else_if_stmt*
   else_clause?

else_if_stmt:
  ELSE IF conditional COLON block


block: 
    NEWLINE INDENT stmts DEDENT

expr:
    INTLIT             { Literal($1)            }
  | BOOLLIT            { BoolLit($1)            }
  | FLOATLIT           { FloatLiteral($1)       }
  | CHARLIT            { CharLiteral($1)        }
  | STRINGLIT          { StringLit($1)          }
  | ID                 { Id($1)                 }
  | expr PLUS   expr   { Binop($1, Add,   $3)   }
  | expr MINUS  expr   { Binop($1, Sub,   $3)   }
  | expr EQ     expr   { Binop($1, Equal, $3)   }
  | expr NEQ    expr   { Binop($1, Neq, $3)     }
  | expr LT     expr   { Binop($1, Less,  $3)   }
  | expr AND    expr   { Binop($1, And,   $3)   }
  | expr OR     expr   { Binop($1, Or,    $3)   }
  | ID ASSIGN expr     { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }