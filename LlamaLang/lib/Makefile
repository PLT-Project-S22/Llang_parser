all:
	ocamlc -w A -c ast.mli
	ocamlyacc parser.mly
	ocamlc -w A -c parser.mli
	ocamlc -w A -c parser.ml
	ocamllex scanner.mll

run: 
	cat test_file.txt | ocaml scanner.ml


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml calc.out calc