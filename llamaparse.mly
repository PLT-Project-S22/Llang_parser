%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN
%token RBRACK LBRACK MULTI DIVIDE MOD FLOOR MOD 
%token INC DEC LTEQ GTEQ NOT
%token EQ NEQ LT AND OR
%token IF ELSE WHILE INT BOOL
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%
