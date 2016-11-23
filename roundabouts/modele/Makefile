all: hello

hello: test

#hello: amodule.cmo bmodule.cmo
#	ocamlc -o hello amodule.cmo bmodule.cmo

test: test.cmo objets.cmo affichage.cmo
	ocamlc -o test objets.cmo affichage.cmo test.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

objets.cmo: objets.ml objets.cmi

affichage.cmo: objets.cmi affichage.ml affichage.cmi

affichage.cmi: affichage.mli objets.cmi

test.cmo: test.ml objets.cmi affichage.cmi
