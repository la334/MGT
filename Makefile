run:
	ocamlbuild -use-ocamlfind -r -pkgs pa_comprehension,ANSITerminal,yojson main.byte && ./main.byte

clean:
	ocamlbuild -clean