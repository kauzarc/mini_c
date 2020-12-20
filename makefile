build:
	ocamlbuild "main.native" -use-menhir -I src/

clean:
	ocamlbuild -clean

menhir:
	ocamlbuild "parser2.native" -use-menhir -I src/