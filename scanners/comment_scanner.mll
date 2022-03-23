{
exception SyntaxError of string
}


rule token = parse
	"/*" {comment lexbuf}
	| eof { exit 0 }

and comment = parse
	"*/" {token lexbuf}
	| _ {comment lexbuf}
	| eof { raise (SyntaxError("Bad Comment"))}


{
	let buf = Lexing.from_channel stdin in 
	let f = token buf in
	print_endline f
}