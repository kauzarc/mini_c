build:
	ocamlbuild "main.native" -use-menhir -I src/

clean:
	ocamlbuild -clean