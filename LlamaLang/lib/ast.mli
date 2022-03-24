(* type op =
  | Add  | Sub   | Mul  | Div  | Flr   | Exp
  | Incr | Decr  | Mod
  | Eq   | Neq   | Lt   | Gt   | Geq   | Leq
  | And  | Or    | Not  | Is   | IsNot 
  | In   | NotIN | Asn  | Pasn 

type typ =
  |Char | String |Bool |Int | Float

type expr =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list
  | Asn of string * expr
  | Seq of expr * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr  * stmt * stmt 
  | While of expr * expr
  | When of expr * (expr * stmt) list
  | Return of expr

type bind = typ * string

type fun_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  local: bind list;
  body: stmt list;
}

type constructor

type program = bind list * fun_def list

type misc = 
| Newline of string
| Dedent of string
| Indent of string
| SingleQuote of string
| DoubleQuote of string
| Backtick of string *)

type holder =
  | Var