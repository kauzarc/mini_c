build:
	ocamlbuild "main.native" -use-menhir -I src/ -verbose 1

clean:
	ocamlbuild -clean