# 1 "scanner2.mll"
 
  open Parser
  open Token

  exception Scanner_error of string
  exception Indentation_error of string

  let create_hash size init =
    let tbl = Hashtbl.create size in
      List.map(fun key data -> Hashtbl.add tbl key data) init;
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
    
    let indentStack = Stack.create()

    let _ = Stack.push 0 indentStack

    let tokenQueue = Queue.create()

    let createIndentationTokens new_indent_level =
      let current_indent_level = Stack.top indentStack in 
        if(new_indent_level > current_indent_level) then (Stack.push new_indent_level indentStack; Queue.push INDENT tokenQueue)
        else ( if(new_indent_level < current_indent_level) then (enqueueDedentTokens(new_indent_level)) )
    
    let enqueueDedentTokens new_indent_level =
      while( new_indent_level < (Stack.top indentStack) ) do
        (let top_level = (Stack.pop indentStack) in 
          if (top_level < new_indent_level) then (raise (Indentation_error("Dendent value does not match a previous indentation level")))
          else Queue.push DEDENT tokenQueue)
      done
    
    let emptyTokenQueue () =
      while(Queue.is_empty tokenQueue = false) do
        Queue.pop tokenQueue
      done 
    let bracket_depth = ref 0

    let countWhiteSpace (whiteSpace: string) = 
      String.fold_left 
        (fun acc c -> if((compare c ' ') == 0) then (acc + 1) else if ((compare c '\t') == 0) then (acc + 4) else acc) 
        0 whiteSpace
    
    let comment_start = ref 0


# 67 "scanner2.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\250\255\251\255\000\000\253\255\254\255\255\255\001\000\
    \252\255\003\000\254\255\005\000\001\000\253\255\254\255\001\000\
    \255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\004\000\255\255\255\255\255\255\000\000\
    \255\255\000\000\255\255\000\000\255\255\255\255\255\255\001\000\
    \255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\255\255\000\000\000\000\000\000\255\255\
    \000\000\010\000\000\000\255\255\014\000\000\000\000\000\255\255\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\006\000\006\000\011\000\007\000\011\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\000\000\000\000\011\000\000\000\011\000\000\000\000\000\
    \000\000\000\000\008\000\015\000\000\000\000\000\000\000\003\000\
    \016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\013\000\000\000\255\255\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\007\000\009\000\000\000\011\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\009\000\255\255\011\000\255\255\255\255\
    \255\255\255\255\003\000\012\000\255\255\255\255\255\255\000\000\
    \015\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\012\000\255\255\009\000\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 78 "scanner2.mll"
                            ( NEWLINE; Lexing.new_line lexbuf; indent lexbuf )
# 170 "scanner2.ml"

  | 1 ->
# 79 "scanner2.mll"
                            ( token lexbuf  )
# 175 "scanner2.ml"

  | 2 ->
# 80 "scanner2.mll"
                            ( COLON         )
# 180 "scanner2.ml"

  | 3 ->
# 81 "scanner2.mll"
                            ( comment_start := lexbuf.lex_curr_p.pos_lnum; comment lexbuf )
# 185 "scanner2.ml"

  | 4 ->
# 82 "scanner2.mll"
                            ( token lexbuf  )
# 190 "scanner2.ml"

  | 5 ->
# 83 "scanner2.mll"
                            ( EOF )
# 195 "scanner2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and indent lexbuf =
   __ocaml_lex_indent_rec lexbuf 9
and __ocaml_lex_indent_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 86 "scanner2.mll"
                            ( if !bracket_depth != 0 then token lexbuf 
                              else (let count = countWhiteSpace (Lexing.lexeme lexbuf in
                                    createIndentationTokens(count)))
                            )
# 210 "scanner2.ml"

  | 1 ->
# 90 "scanner2.mll"
                            ( (emptyTokenQueue() ; token lexbuf))
# 215 "scanner2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_indent_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 12
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 93 "scanner2.mll"
         ( token lexbuf   )
# 227 "scanner2.ml"

  | 1 ->
# 94 "scanner2.mll"
         ( comment lexbuf )
# 232 "scanner2.ml"

  | 2 ->
# 95 "scanner2.mll"
         ( raise (Scanner_error("Comment not closed")))
# 237 "scanner2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

