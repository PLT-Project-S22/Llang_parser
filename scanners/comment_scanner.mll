{
exception SyntaxError of string
}

let left = '/' '*'
let right = '*' '/'
let any = ['^'left right]*
let comment = left any right


rule lex_float = parse
	comment {Lexing.lexeme lexbuf}
	| _ { raise (SyntaxError ("bad comment")) }
	| eof { exit 0 }


{
	let buf = Lexing.from_channel stdin in 
	let f = lex_float buf in
	print_endline f
}