all: hello

hello: test

#hello: amodule.cmo bmodule.cmo
#	ocamlc -o hello amodule.cmo bmodule.cmo

test: objets.cmo affichage.cmo comportements.cmo plateau.cmo test.cmo
	ocamlc -o test graphics.cma objets.cmo affichage.cmo comportements.cmo plateau.cmo test.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

depend:
	ocamldep *.ml *.mli >.depend

sinclude .depend


