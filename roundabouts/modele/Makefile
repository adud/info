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

objets.cmo: objets.ml objets.cmi

affichage.cmo: objets.cmi affichage.ml affichage.cmi

affichage.cmi: affichage.mli objets.cmi

comportements.cmo : comportements.cmi objets.cmi comportements.ml

comportements.cmi : comportements.mli objets.cmi

plateau.cmo : plateau.ml plateau.cmi objets.cmi affichage.cmi comportements.cmi

plateau.cmi : plateau.ml objets.cmi affichage.cmi comportements.cmi

test.cmo: test.ml objets.cmi affichage.cmi comportements.cmi
