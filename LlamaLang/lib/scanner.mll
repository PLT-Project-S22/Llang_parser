{
  open Parser
  exception Scanner_error of string
  exception Indentation_error of string

  let create_hash size init =
    let tbl = Hashtbl.create size in
      List.iter(fun (key, data) -> Hashtbl.add tbl key data) init;
      tbl

 let keywords = 
     [ "and", AND;             "or", OR;                   "not", NOT;
       "const", CONST;         "final", FINAL;             "is", IS;
       "is not", ISNOT;        "in", IN;                   "not in", NOTIN; 
       "when", WHEN;           "then", THEN;               "switch", SWITCH;
       "case", CASE;           "default", DEFAULT;         "do", DO;
       "while", WHILE;         "if", IF;                   "else", ELSE; 
       "break", BREAK;         "continue", CONTINUE;       "for", FOR;
       "class", CLASS;         "constructor", CONSTRUCTOR; "new", NEW;
       "super", SUPER;         "extends", EXTENDS;         "implements", IMPLEMENTS;
       "this", THIS;           "interface", INTERFACE;     "throws", THROWS;
       "raises", RAISES;       "try", TRY;                 "catch", CATCH;
       "finally", FINALLY;     "throw", THROW;             "raise", RAISE;
       "return", RETURN;       "void", VOID;               "bool", BOOL;
       "int", INT;             "float", FLOAT;             "char", CHAR;
       "string", STRING;       "null", NULL;               "true", TRUE;
       "false", FALSE;         "import", IMPORT;           "as", AS ]

    let symbol_tbl = create_hash 48 keywords
    let id_tbl = Hashtbl.create 0
    
    let indentStack = Stack.create()

    let _ = Stack.push 0 indentStack

    let tokenQueue = Queue.create()
    let bracketsList =
      ['[', ']';
       '{', '}';
       '(', ')' ]
    let bracketTbl = create_hash 3 bracketsList
    let bracketStack = Stack.create ()

    let inBrackets = ref false

    let rec enqueueDedentTokens new_indent_level =
      if (new_indent_level < (Stack.top indentStack)) then 
        (let top_level = (Stack.pop indentStack) in
          if(top_level < new_indent_level) then (raise (Indentation_error("Dendent value does not match a previous indentation level") ))
          else (Queue.push DEDENT tokenQueue; enqueueDedentTokens(new_indent_level)) )

    let createIndentationTokens new_indent_level =
      let current_indent_level = Stack.top indentStack in 
        if(new_indent_level > current_indent_level) then ( Stack.push new_indent_level indentStack; Queue.push INDENT tokenQueue)
        else ( if(new_indent_level < current_indent_level) then (enqueueDedentTokens(new_indent_level) ) )
    
    let countWhiteSpace (whiteSpace: string) = 
      String.fold_left 
        (fun acc c -> if((compare c ' ') == 0) then (acc + 1) else if ((compare c '\t') == 0) then (acc + 4) else acc) 
        0 whiteSpace
    
    let commentStart = ref 0

    let balancedBracket currBracket =
      if(Stack.is_empty bracketStack) then raise(Scanner_error("Mismatched Brackets"))
      else 
        if((compare (Stack.top bracketStack) currBracket) != 0) then raise(Scanner_error("Mismatched Brackets"))
        else (ignore(Stack.pop bracketStack); true)

}

let digit = ['0'-'9']
let integer = ('-' | '+')?digit+

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase

let ident = ('_' '$' letter)?(letter | digit | '_' )+
let newline = ('\r'?'\n')|('\r')
let startingWhiteSpace = ['\t' ' ']* 

rule token = parse 
  | newline                 {  Queue.add NEWLINE tokenQueue; indent lexbuf }
  | [' ' '\t']              {  token lexbuf  }
  | ':'                     {  COLON         }
  | "/*"                    {  comment lexbuf}
  | eof                     {  EOF           }
  | '+'                     {  PLUS          }
  | '-'                     {  MINUS         }
  | '*'                     {  TIMES         }
  | '/'                     {  DIVIDE        }
  | '%'                     {  MODULO        }
  | '^'                     {  EXPON         }
  | "//"                    {  FLOOR         }
  | "++"                    {  INCREMENT     }
  | "--"                    {  DECREMENT     }
  | '='                     {  ASSIGN        }
  | "+="                    {  PLUSASSIGN    }
  | "-="                    {  MINUSASSIGN   }
  | "*="                    {  TIMESASSIGN   }
  | "/="                    {  DIVIDEASSIGN  }
  | "%="                    {  MODULOASSIGN  }
  | "//="                   {  FLOORASSIGN   }
  | "^="                    {  EXPONASSIGN   }
  | "&&"                    {  LAND          }
  | "||"                    {  LOR           }
  | '!'                     {  LNOT          }
  | "=="                    {  EQ            }
  | "!="                    {  NEQ           }
  | '>'                     {  GT            }
  | '<'                     {  LT            }
  | '.'                     {  DOT           }
  | ';'                     {  SEMICOLON     }
  | ':'                     {  COLON         }
  | '('                     {  LPAREN        }
  | ')'                     {  RPAREN        }
  | '['                     {  LBRACKET      }
  | ']'                     {  RBRACKET      }
  | '{'                     {  LBRACE        }
  | '}'                     {  RBRACE        }
  | ','                     {  COMMA         }
  | '''                     {  SINGLEQUOTE   }
  | '"'                     {  DOUBLEQUOTE   }
  | '`'                     {  BACKTICK      }
  | ['0'-'9']+ as lem       {  INTLIT(int_of_string(lem))}
  | ident as id             {  if(Hashtbl.mem symbol_tbl id) then Hashtbl.find symbol_tbl id
                               else (ID(id))
                            }



and indent = parse
  | startingWhiteSpace     { let count = countWhiteSpace(Lexing.lexeme lexbuf) in
                            createIndentationTokens count; Queue.take tokenQueue;
                           }

and comment = parse
  | "*/" { token lexbuf }
  | eof  { raise(Scanner_error("non-terminated comment"))}
  | _    { comment lexbuf }

{

  let rec tokenize lexbuf = 
    let parsed_token =
      if (Queue.is_empty tokenQueue = false) then Queue.take tokenQueue
      else token lexbuf in
        match parsed_token with
        | NEWLINE      -> print_string "NEWLINE \n"   ; tokenize lexbuf
        | DEDENT       -> print_string "DEDENT "      ; tokenize lexbuf
        | INDENT       -> print_string "INDENT "      ; tokenize lexbuf
        | PLUS         -> print_string "PLUS "        ; tokenize lexbuf   
        | MINUS        -> print_string "MINUS "       ; tokenize lexbuf   
        | TIMES        -> print_string "TIMES "       ; tokenize lexbuf   
        | DIVIDE       -> print_string "DIVIDE "      ; tokenize lexbuf   
        | MODULO       -> print_string "MODULO "      ; tokenize lexbuf   
        | EXPON        -> print_string "EXPON "       ; tokenize lexbuf   
        | FLOOR        -> print_string "FLOOR "       ; tokenize lexbuf   
        | INCREMENT    -> print_string "INCREMENT "   ; tokenize lexbuf   
        | DECREMENT    -> print_string "DECREMENT "   ; tokenize lexbuf   
        | ASSIGN       -> print_string "ASSIGN "      ; tokenize lexbuf   
        | PLUSASSIGN   -> print_string "PLUSASSIGN "  ; tokenize lexbuf   
        | MINUSASSIGN  -> print_string "MINUSASSIGN " ; tokenize lexbuf   
        | TIMESASSIGN  -> print_string "TIMESASSIGN " ; tokenize lexbuf   
        | DIVIDEASSIGN -> print_string "DIVIDEASSIGN "; tokenize lexbuf   
        | MODULOASSIGN -> print_string "MODULOASSIGN "; tokenize lexbuf   
        | FLOORASSIGN  -> print_string "FLOORASSIGN " ; tokenize lexbuf   
        | EXPONASSIGN  -> print_string "EXPONASSIGN " ; tokenize lexbuf   
        | LAND         -> print_string "LAND "        ; tokenize lexbuf   
        | LOR          -> print_string "LOR "         ; tokenize lexbuf   
        | LNOT         -> print_string "LNOT "        ; tokenize lexbuf   
        | EQ           -> print_string "EQ "          ; tokenize lexbuf   
        | NEQ          -> print_string "NEQ "         ; tokenize lexbuf   
        | GT           -> print_string "GT "          ; tokenize lexbuf   
        | LT           -> print_string "LT "          ; tokenize lexbuf   
        | DOT          -> print_string "DOT "         ; tokenize lexbuf   
        | SEMICOLON    -> print_string "SEMICOLON "   ; tokenize lexbuf   
        | COLON        -> print_string "COLON "       ; tokenize lexbuf   
        | LPAREN       -> print_string "LPAREN "      ; tokenize lexbuf   
        | RPAREN       -> print_string "RPAREN "      ; tokenize lexbuf   
        | LBRACKET     -> print_string "LBRACKET "    ; tokenize lexbuf   
        | RBRACKET     -> print_string "RBRACKET "    ; tokenize lexbuf   
        | LBRACE       -> print_string "LBRACE "      ; tokenize lexbuf   
        | RBRACE       -> print_string "RBRACE "      ; tokenize lexbuf   
        | COMMA        -> print_string "COMMA "       ; tokenize lexbuf   
        | SINGLEQUOTE  -> print_string "SINGLEQUOTE " ; tokenize lexbuf   
        | DOUBLEQUOTE  -> print_string "DOUBLEQUOTE " ; tokenize lexbuf   
        | BACKTICK     -> print_string "BACKTICK "    ; tokenize lexbuf
        | AND          -> print_string "AND "         ; tokenize lexbuf
        | OR           -> print_string "OR "    ; tokenize lexbuf
        | NOT          -> print_string "NOT "    ; tokenize lexbuf
        | CONST        -> print_string "CONST "    ; tokenize lexbuf
        | FINAL        -> print_string "FINAL "    ; tokenize lexbuf
        | IS           -> print_string "IS "    ; tokenize lexbuf
        | ISNOT        -> print_string "ISNOT "    ; tokenize lexbuf
        | IN           -> print_string "IN "    ; tokenize lexbuf
        | NOTIN        -> print_string "NOTIN "; tokenize lexbuf
        | WHEN         -> print_string "WHEN "; tokenize lexbuf
        | WHILE        -> print_string "WHILE "; tokenize lexbuf
        | IF           -> print_string "IF "; tokenize lexbuf
        | ELSE         -> print_string "ELSE "; tokenize lexbuf
        | BREAK        -> print_string "BREAK "; tokenize lexbuf
        | CONTINUE     -> print_string "CONTINUE "; tokenize lexbuf
        | DO           -> print_string "DO "; tokenize lexbuf
        | FOR          -> print_string "FOR "; tokenize lexbuf
        | THEN         -> print_string "THEN "; tokenize lexbuf
        | SWITCH       -> print_string "SWITCH "; tokenize lexbuf
        | CASE         -> print_string "CASE "; tokenize lexbuf
        | DEFAULT      -> print_string "DEFAULT "; tokenize lexbuf
        | CLASS        -> print_string "CLASS "; tokenize lexbuf
        | CONSTRUCTOR  -> print_string "CONSTRUCTOR "; tokenize lexbuf
        | NEW          -> print_string "NEW "; tokenize lexbuf
        | SUPER        -> print_string "SUPER "; tokenize lexbuf
        | EXTENDS      -> print_string "IN "; tokenize lexbuf
        | IMPLEMENTS   -> print_string "IMPLEMENTS "; tokenize lexbuf
        | DOT          -> print_string "DOT "; tokenize lexbuf
        | INTERFACE    -> print_string "INTERFACE "; tokenize lexbuf
        | THROWS       -> print_string "THROWS "; tokenize lexbuf
        | RAISES       -> print_string "RAISES "; tokenize lexbuf
        | THIS         -> print_string "THIS "; tokenize lexbuf
        | BOOL         -> print_string "BOOL "; tokenize lexbuf
        | INT          -> print_string "INT "; tokenize lexbuf
        | FLOAT        -> print_string "FLOAT "; tokenize lexbuf
        | CHAR         -> print_string "CHAR "; tokenize lexbuf
        | STRING       -> print_string "STRING "; tokenize lexbuf
        | NULL         -> print_string "NULL "; tokenize lexbuf
        | TRUE         -> print_string "TRUE "; tokenize lexbuf
        | FALSE        -> print_string "FALSE "; tokenize lexbuf
        | IMPORT       -> print_string "IMPORT "; tokenize lexbuf
        | AS           -> print_string "AS "; tokenize lexbuf
        | RETURN       -> print_string "RETURN "; tokenize lexbuf
        | VOID         -> print_string "VOID "; tokenize lexbuf
        | TRY          -> print_string "TRY "; tokenize lexbuf
        | CATCH        -> print_string "CATCH "; tokenize lexbuf
        | FINALLY      -> print_string "FINALLY "; tokenize lexbuf
        | THROW        -> print_string "THROW "; tokenize lexbuf
        | RAISE        -> print_string "RAISE "; tokenize lexbuf
        | EOF          -> print_string "EOF " ; parsed_token;
        | _            -> tokenize lexbuf;
  in
  let buf = Lexing.from_channel stdin in
     tokenize buf
}