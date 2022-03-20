{ 
  open Parser 
  
  module Stack = Stack.create(struct
                              type t = int
                              )
}


let digit = ['0'-'9']
let integer = ('-' | '+')?digit+

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase

let id = (_ letter) (letter | digit)+
let newline = ['\n' '\r']$
rule token = parse
  | [' ' '\t']              { token lexbuf  }
  | newline                 { NEWLINE }
  | '\t'                    { indent lexbuf }
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
  | "while"                 { WHEN          }
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
  | '\t' {}
  | _    { token lexbuf }