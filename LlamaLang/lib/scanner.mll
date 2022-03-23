{ 
  open Parser 


  let create_hashtbl = 
  (* stack containing levels of indentation \t is treated as 4 whitespaces like python *)
  let indentStack = Stack.create()
  (* initial indentation level *)
  Stack.push 0 indentStack
  (* will hold dispersement of INDENT DEDENT tokens *)
  let indentDedentQueue = Queue.create()
  (* mutable counter variable set to initial indentation level of 0 *)
  let whiteSpaceCounter = ref 0
  (* mutable boolean variable set to false -- different indentation rules in brackets *)
  let insideOfBrackets = ref false
  (* counts the white space in a string acc is incremented by 1 for \s and 4 for \t*)
  let countWhiteSpace (whiteSpace: string) =
    whiteSpaceCounter := String.fold_left (fun acc c -> if((compare c ' ') == 0) then (acc + 1) else if ((compare c '\t') == 0) then (acc + 4) else acc) 0 whiteSpace
  (* resets the white space counter pointer to 0 *)
  let resetCounter () = whiteSpaceCounter := 0
  (* adds indentation level to stack if current indent level is greater than top of stack else will pop all values off of stack until countWhiteSpace is no longer greater *)
  (* TODO -- finish logic for all scenarios and handle edge cases *)
  let indentDedentTokens () = 
    if (!whiteSpaceCounter > Stack.top indentStack ) then begin Stack.push !whiteSpaceCounter indentStack; Queue.push INDENT indentDedentQueue; ignore(resetCounter()) end
    else if (!whiteSpaceCounter < Stack.top indentStack) then begin popIndentations() ; ignore(resetCounter()) end else ignore(resetCounter());
  
  let enqueueDedents (dedents: int) =
    for i = 0 to !dedents do Queue.push DEDENT indentDedentQueue done
    let i = ref 0 in while !i < dedents do Queue.push DEDENT indentDedentQueue; ignore(incr i); done
  
  let dedents = ref 0
  let resetDedents () = dedents := 0
  
  let popIndentations () = while !whiteSpaceCounter > Stack.top indentStack do begin Stack.pop indentStack; incr dedents end done; enqueueDedents !dedents; ignore(resetDedents());
  
  (* 
    TODO - account for whitespace logic. PseudoCode should be as follows
    1. create a stack to track indentation levels
    2. add 0 to stack as the inital indentation level
    3. from start of file until first NEWLINE regex there should not be any indentation unless in brackets
      if indentation changes from 0 throw an error
    4. if NEWLINE regex is found return NEWLINE and look for the STARTINGWHITESPACE regex
    5. count all white space (\s = 1, \t = 4) in STARTINGWHITESPACE
    6. if count is greater than top of stack add current count to stack, reset count, return INDENT token
       else if count < top of stack pop every value greater than count - for every value popped from stack return DEDENT TOKEN - reset count
       else reset count
    7. if inside any number of brack icons ( ( ) [ ] { } ) do not track indentation level
    8. if inside comments ( /* */ ) do not track indentation level
    9. first line of python code cannot be indented 
   *)
}


let digit = ['0'-'9']
let integer = ('-' | '+')?digit+

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase

let id = (_ letter) (letter | digit | _ )+
let newline = ['\n' '\r' "\r\n" ]$
let startingWhiteSpace = ['\t' ' ']*

rule token = parse
  | [' ' '\t' '\n' '\r']    { token lexbuf  }
  | newline                 { NEWLINE; indent lexbuf }
  | '+'                     { PLUS          }
  | '-'                     { MINUS         }
  | '*'                     { TIMES         }
  | '/'                     { DIVIDE        }
  | '%'                     { MODULO        }
  | '^'                     { EXPON         }
  | "//"                    { FLOOR         }
  | "++"                    { INCREMENT     }
  | "--"                    { DECREMENT     }
  | '='                     { ASSIGN        }
  | "+="                    { PLUSASSIGN    }
  | "-="                    { MINUSASSIGN   }
  | "*="                    { TIMESASSIGN   }
  | "/="                    { DIVIDEASSIGN  }
  | "%="                    { MODULOASSIGN  }
  | "//="                   { FLOORASSIGN   }
  | "^="                    { EXPONASSIGN   }
  | "&&"                    { LAND          }
  | "||"                    { LOR           }
  | '!'                     { LNOT          }
  | "=="                    { EQ            }
  | "!="                    { NEQ           }
  | '>'                     { GT            }
  | '<'                     { LT            }
  | "<="                    { GEQ           }
  | ">="                    { LEQ           }
  | "and"                   { AND           }
  | "or"                    { OR            }
  | "not"                   { NOT           }
  | "const"                 { CONST         }
  | "final"                 { FINAL         }
  | "is"                    { IS            }
  | "is not"                { ISNOT         }
  | "in"                    { IN            }
  | "not in"                { NOTIN         }
  | "when"                  { WHEN          }
  | "while"                 { WHILE         }
  | "if"                    { IF            }
  | "else"                  { ELSE          }
  | "break"                 { BREAK         }
  | "continue"              { CONTINUE      }
  | "do"                    { DO            }
  | "for"                   { FOR           }
  | "then"                  { THEN          }
  | "switch"                { SWITCH        }
  | "case"                  { CASE          }
  | "default"               { DEFAULT       }
  | "class"                 { CLASS         }
  | "constructor"           { CONSTRUCTOR   }
  | "new"                   { NEW           }
  | "super"                 { SUPER         }
  | "extends"               { EXTENDS       }
  | "implements"            { IMPLEMENTS    }
  | '.'                     { DOT           }
  | "interface"             { INTERFACE     }
  | "throws"                { THROWS        }
  | "raises"                { RAISES        }
  | "this"                  { THIS          }
  | "bool"                  { BOOL          }
  | "int"                   { INT           }
  | "float"                 { FLOAT         }
  | "char"                  { CHAR          }
  | "string"                { STRING        }
  | "null"                  { NULL          }
  | "true"                  { TRUE          }
  | "false"                 { FALSE         }
  | "import"                { IMPORT        }
  | "as"                    { AS            }
  | "return"                { RETURN        }
  | "void"                  { VOID          }
  | "try"                   { TRY           }
  | "catch"                 { CATCH         }
  | "finally"               { FINALLY       }
  | "throw"                 { THROW         }
  | "raise"                 { RAISE         }
  | ';'                     { SEMICOLON     }
  | ':'                     { COLON         }
  | "/*"                    { LCOMMENT      }
  | "*/"                    { RCOMMENT      }
  | '('                     { LPAREN        }
  | ')'                     { RPAREN        }
  | '['                     { LBRACKET      }
  | ']'                     { RBRACKET      }
  | '{'                     { LBRACE        }
  | '}'                     { RBRACE        }
  | ','                     { COMMA         }
  | '''                     { SINGLEQUOTE   }
  | '"'                     { DOUBLEQUOTE   }
  | '`'                     { BACKTICK      }

and indent = parse
  | startingWhiteSpace { countWhiteSpace (Lexing.lexeme lexbuf) }
  | _    { indentDedentTokens ; token lexbuf }