{
exception SyntaxError of string
}

let true = "true"
let false = "false"

rule token = parse
	true {Lexing.lexeme lexbuf}
	| false {Lexing.lexeme lexbuf}
	| _ { raise (SyntaxError("Not a Boolean token")) }
	| eof { exit 0 }

{
	let buf = Lexing.from_channel stdin in 
	let f = token buf in
	print_endline f
}