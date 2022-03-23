{
  exception Scanner_error of string
  exception Indentation_error of string

  let create_hash size init =
    let tbl = Hashtbl.create size in
      ignore(List.map(fun key data -> Hashtbl.add tbl key data) init);
      tbl

  let keywords = 
    [ "and", "AND";             "or", "OR";                   "not", "NOT";
      "const", "CONST";         "final", "FINAL";             "is", "IS";
      "is not", "ISNOT";        "in", "IN";                   "not in", "NOTIN"; 
      "when", "WHEN";           "then", "THEN";               "switch", "SWITCH";
      "case", "CASE";           "default", "DEFAULT";         "do", "DO";
      "while", "WHILE";         "if", "IF";                   "else", "ELSE"; 
      "break", "BREAK";         "continue", "CONTINUE";       "for", "FOR";
      "class", "CLASS";         "constructor", "CONSTRUCTOR"; "new", "NEW";
      "super", "SUPER";         "extends", "EXTENDS";         "implements", "IMPLEMENTS";
      "this", "THIS";           "interface", "INTERFACE";     "throws", "THROWS";
      "raises", "RAISES";       "try", "TRY";                 "catch", "CATCH";
      "finally", "FINALLY";     "throw", "THROW";             "raise", "RAISE";
      "return", "RETURN";       "void", "VOID";               "bool", "BOOL";
      "int", "INT";             "float", "FLOAT";             "char", "CHAR";
      "string", "STRING";       "null", "NULL";               "true", "TRUE";
      "false", "FALSE";         "import", "IMPORT";           "as", "AS" ]

    let symbol_tbl = create_hash 48 keywords
    let id_tbl = Hashtbl.create 0
    
    let indentStack = Stack.create()

    let _ = Stack.push 0 indentStack

    let tokenQueue = Queue.create()

    let rec enqueueDedentTokens new_indent_level =
      if (new_indent_level < (Stack.top indentStack)) then 
        (let top_level = (Stack.pop indentStack) in
          if(top_level < new_indent_level) then (raise (Indentation_error("Dendent value does not match a previous indentation level") ))
          else (Queue.push "DEDENT " tokenQueue; enqueueDedentTokens(new_indent_level)) )

    let createIndentationTokens new_indent_level =
      let current_indent_level = Stack.top indentStack in 
        if(new_indent_level > current_indent_level) then ( Stack.push new_indent_level indentStack; Queue.push "INDENT " tokenQueue)
        else ( if(new_indent_level < current_indent_level) then (enqueueDedentTokens(new_indent_level) ) )
    
    
    (* let emptyTokenQueue () =
      while(Queue.is_empty tokenQueue = false) do
        Queue.pop tokenQueue
      done  *)
    
    let bracket_depth = ref 0

    let countWhiteSpace (whiteSpace: string) = 
      String.fold_left 
        (fun acc c -> if((compare c ' ') == 0) then (acc + 1) else if ((compare c '\t') == 0) then (acc + 4) else acc) 
        0 whiteSpace
    
    let comment_start = ref 0

        
    let readQueue() =
      while((Queue.is_empty tokenQueue) = false) do
        print_string (Queue.take tokenQueue)
      done
    

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
  | newline                 { print_string "NEWLINE \n"; indent lexbuf }
  | [' ' '\t']              { token lexbuf  }
  | ':'                     { print_string "COLON "; token lexbuf }
  | "/*"                    { print_string "LCOMMENT "; comment lexbuf}
  | eof                     { print_string "EOF";}
  | '+'                     { print_string "PLUS "        ;  token lexbuf   }
  | '-'                     { print_string "MINUS "       ;  token lexbuf   }
  | '*'                     { print_string "TIMES "       ;  token lexbuf   }
  | '/'                     { print_string "DIVIDE "      ;  token lexbuf   }
  | '%'                     { print_string "MODULO "      ;  token lexbuf   }
  | '^'                     { print_string "EXPON "       ;  token lexbuf   }
  | "//"                    { print_string "FLOOR "       ;  token lexbuf   }
  | "++"                    { print_string "INCREMENT "   ;  token lexbuf   }
  | "--"                    { print_string "DECREMENT "   ;  token lexbuf   }
  | '='                     { print_string "ASSIGN "      ;  token lexbuf   }
  | "+="                    { print_string "PLUSASSIGN "  ;  token lexbuf   }
  | "-="                    { print_string "MINUSASSIGN " ;  token lexbuf   }
  | "*="                    { print_string "TIMESASSIGN " ;  token lexbuf   }
  | "/="                    { print_string "DIVIDEASSIGN ";  token lexbuf   }
  | "%="                    { print_string "MODULOASSIGN ";  token lexbuf   }
  | "//="                   { print_string "FLOORASSIGN " ;  token lexbuf   }
  | "^="                    { print_string "EXPONASSIGN " ;  token lexbuf   }
  | "&&"                    { print_string "LAND "        ;  token lexbuf   }
  | "||"                    { print_string "LOR "         ;  token lexbuf   }
  | '!'                     { print_string "LNOT "        ;  token lexbuf   }
  | "=="                    { print_string "EQ "          ;  token lexbuf   }
  | "!="                    { print_string "NEQ "         ;  token lexbuf   }
  | '>'                     { print_string "GT "          ;  token lexbuf   }
  | '<'                     { print_string "LT "          ;  token lexbuf   }
  | '.'                     { print_string "DOT "         ;  token lexbuf   }
  | ';'                     { print_string "SEMICOLON "   ;  token lexbuf   }
  | ':'                     { print_string "COLON "       ;  token lexbuf   }
  | '('                     { print_string "LPAREN "      ;  token lexbuf   }
  | ')'                     { print_string "RPAREN "      ;  token lexbuf   }
  | '['                     { print_string "LBRACKET "    ;  token lexbuf   }
  | ']'                     { print_string "RBRACKET "    ;  token lexbuf   }
  | '{'                     { print_string "LBRACE "      ;  token lexbuf   }
  | '}'                     { print_string "RBRACE "      ;  token lexbuf   }
  | ','                     { print_string "COMMA "       ;  token lexbuf   }
  | '''                     { print_string "SINGLEQUOTE " ;  token lexbuf   }
  | '"'                     { print_string "DOUBLEQUOTE " ;  token lexbuf   }
  | '`'                     { print_string "BACKTICK "    ;  token lexbuf   }
  | ['0'-'9']+              { print_string "INTLIT "       ;  token lexbuf   }
  | ident as id             { print_string (id) ;  token lexbuf }



and indent = parse
  | startingWhiteSpace     { let count = countWhiteSpace(Lexing.lexeme lexbuf) in
                            createIndentationTokens count; readQueue(); token lexbuf;
                           }
  | _                      { let count = countWhiteSpace(Lexing.lexeme lexbuf) in
                            createIndentationTokens count; readQueue(); token lexbuf }

and comment = parse
  | "*/" { print_string "RCOMMENT "; token lexbuf }
  | _    { comment lexbuf }
  | eof  { print_string "EOF"          }

{
  let buf = Lexing.from_channel stdin in
    token buf
}