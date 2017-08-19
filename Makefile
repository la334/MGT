run:
	ocamlbuild -use-ocamlfind -r -pkgs pa_comprehension,ANSITerminal,yojson mgt.byte && ./mgt.byte

clean:
	ocamlbuild -clean