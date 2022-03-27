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
       "const", CONST;         "is", IS;
       "is not", ISNOT;        "in", IN;                   "not in", NOTIN; 
       "when", WHEN;           "default", DEFAULT;         
       "while", WHILE;         "if", IF;                   "else", ELSE; 
       "break", BREAK;         "continue", CONTINUE;       "for", FOR;
       "class", CLASS;         "constructor", CONSTRUCTOR; "new", NEW;
       "super", SUPER;         "extends", EXTENDS;         "implements", IMPLEMENTS;
       "this", THIS;           "interface", INTERFACE;     "throws", THROWS;
       "try", TRY;             "catch", CATCH;
       "finally", FINALLY;     "throw", THROW;             
       "return", RETURN;       "void", VOID;               "bool", BOOL;
       "int", INT;             "float", FLOAT;             "char", CHAR;
       "string", STRING;       "null", NULL;               "true", TRUE;
       "false", FALSE;         "import", IMPORT;           "as", AS ]

    let symbol_tbl = create_hash 48 keywords
    
    let indentStack = Stack.create()

    let _ = Stack.push 0 indentStack

    let tokenQueue = Queue.create()

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


}

let digit = ['0'-'9']
let integer = ('-' | '+')?digit+

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase

let ident = ('_' '$' letter)?(letter | digit | '_' )+
let newline = '\n' | '\r' | "\r\n"
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
        | EOF          -> print_string "EOF " ; parsed_token;
        | _            -> tokenize lexbuf;
  in
  let buf = Lexing.from_channel stdin in
     tokenize buf
}