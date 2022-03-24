all:
	ocamlc -w A -c ast.mli
	ocamlyacc parser.mly
	ocamlc -w A -c parser.mli
	ocamlc -w A -c parser.ml
	ocamllex scanner.mll

run: 
	cat test_file.txt | ocaml scanner.ml

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml calc.out calc