(* Ocamllex scanner for MicroC *)

{ open Llamaparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let integer = ('-' | '+')?digit+
let float = digit*['.']digit* 
let string = '"' letter* '"'
let char = ''' '.' '''

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
(* COMMA *)
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
(* RETURN *)
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "float" { FLOAT }
| "char"  { CHAR }
| "string" { STRING }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| float as lem { FLIT(Float.of_string lem) }
| string as lem { STRLIT(lem)}
| char as lem { CHARLIT(lem.[1])}
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }