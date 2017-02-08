all: test local sature
#hello: amodule.cmo bmodule.cmo
#	ocamlc -o hello amodule.cmo bmodule.cmo

cmo:=objets.cmo affichage.cmo comportements.cmo plateau.cmo

test: ${cmo} test.cmo
	ocamlc -o test unix.cma graphics.cma ${cmo} test.cmo

sature: ${cmo} sature.cmo
	ocamlc -o sature unix.cma graphics.cma ${cmo} sature.cmo

local: ${cmo} local.cmo
	ocamlc -o local unix.cma graphics.cma ${cmo} local.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

depend:
	ocamldep *.ml *.mli >.depend

sinclude .depend


