all: hello

hello: test

#hello: amodule.cmo bmodule.cmo
#	ocamlc -o hello amodule.cmo bmodule.cmo

test: objets.cmo affichage.cmo comportements.cmo test.cmo
	ocamlc -o test objets.cmo affichage.cmo comportements.cmo test.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

objets.cmo: objets.ml objets.cmi

affichage.cmo: objets.cmi affichage.ml affichage.cmi

affichage.cmi: affichage.mli objets.cmi

comportements.cmo : comportements.cmi objets.cmi comportements.ml

comportements.cmi : comportements.mli objets.cmi

test.cmo: test.ml objets.cmi affichage.cmi comportements.cmi
