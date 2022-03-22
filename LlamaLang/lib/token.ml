type token =
  (* BEGIN WHITE SPACE DELIMITERS *)
  NEWLINE | INDENT      | DEDENT      |
  (* BEGIN ARITHMETIC OPERATORS*)
  PLUS    | MINUS       | TIMES       | DIVIDE       | MODULO      | EXPON        |
  FLOOR   | INCREMENT   | DECREMENT   |
  (* BEGIN ASSIGNMENT OPERATORS *)
  ASSIGN  | PLUSASSIGN  | MINUSASSIGN | DIVIDEASSIGN | TIMESASSIGN | MODULOASSIGN | FLOORASSIGN | EXPONASSIGN |
  (* BEGIN LOGICAL OPERATORS *)
  LAND    | LOR         | LNOT        |
  (* BEGIN COMPARISON OPERATORS *)
  EQ      | NEQ         | GT          | LT           | GEQ         | LEQ          |
  (* BEGIN KEYWORDS *)
  AND     | OR          | NOT         |
  CONST   | FINAL       |
  IS      | ISNOT       | IN          | NOTIN        |
  WHEN    | THEN        | SWITCH      | CASE         | DEFAULT     |
  DO      | WHILE       | IF          | ELSE         | BREAK       | CONTINUE      | FOR        |
  CLASS   | CONSTRUCTOR | NEW         | SUPER        | EXTENDS     | IMPLEMENTS    | INTERFACE  |
  THIS    | THROWS      | RAISES      |
  TRY     | CATCH       | FINALLY     | THROW        | RAISE       |
  RETURN  | VOID        | 
  BOOL    | INT         | FLOAT       | CHAR         | STRING      | NULL          |
  TRUE    | FALSE       |
  IMPORT  | AS          |
  (* BEGIN CHARACTER DELIMITERS *)
  COLON   | SEMICOLON   | LPAREN      | RPAREN       | LBRACKET    | RBRACKET      | LBRACE     | RBRACE       |
  COMMA   | SINGLEQUOTE | DOUBLEQUOTE | BACKTICK     | DOT    