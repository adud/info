all: hello

hello: test

#hello: amodule.cmo bmodule.cmo
#	ocamlc -o hello amodule.cmo bmodule.cmo

test: test.cmo objets.cmo affichage.cmo
	ocamlc -o test test.cmo objets.cmo affichage.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

objets.cmo: objets.ml objets.cmi

affichage.cmo: objets.cmo objets.cmi affichage.ml affichage.cmi

affichage.cmi: affichage.mli objets.cmo objets.cmi

test.cmo: test.ml objets.cmo objets.cmi affichage.cmo affichage.cmi