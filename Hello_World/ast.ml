type op =
  | Add  | Sub   | Mul  | Div  | Flr   | Exp
  | Inc  | Dec   | Mod
  | Eq    | Neq  | Lt   | Gt   | Geq   | Leq
  | LAnd | LOr   | LNot 
  | And  | Or    | Not  | Is   | IsNot 
  | In   | NotIn 

type typ =
  | Char | String | Bool | Int | Float | Void

type bind = typ * string

type holder = Var

type expr =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list
  | Assign of string * expr


type stmt = 
  | Block of stmt list
  | Simple_stmts of stmt list
  | Return of expr
  | Import of string
  | Expr of expr
  | Throw of expr
  | If_stmt of expr * stmt  * ((stmt) option)
  | For_in of typ * expr * expr * stmt
  | For of expr list * stmt
  | When of expr * stmt
  | While of expr * stmt
  | Case of expr * stmt
  | Default of stmt
  | Try of stmt * stmt * ((stmt) option)
  | Func of typ * expr * expr option * stmt

type program = stmt list








(* start of printing operations *)

let string_of_op = function
  | Add   -> "+"
  | Sub   -> "-"
  | Mul   -> "*"
  | Div   -> "/"
  | Flr   -> "//"
  | Exp   -> "^"
  | Inc   -> "++"
  | Dec   -> "--"
  | Mod   -> "%"
  | Eq    -> "=="
  | Neq   -> "!="
  | Gt    -> ">"
  | Lt    -> "<"
  | Geq   -> ">="
  | Leq   -> "<="
  | LAnd  -> "&&"
  | LOr   -> "||"
  | LNot  -> "!"
  | And   -> "and"
  | Or    -> "or"
  | Not   -> "not"
  | Is    -> "is"
  | IsNot -> "is not"
  | In    -> "in"
  | NotIn -> "not in"

let string_of_typ = function
  Int -> "int"
| Bool -> "bool"
| Char -> "char"
| String -> "string"
| Float -> "float"
| Void -> "void"

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> string_of_float f
  | CharLit(c)  -> String.make 1 c
  | StringLit(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

(* let rec string_of_stmt = function
  Expr(expr) -> string_of_expr expr ^ "\n" *)
