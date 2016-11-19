all: hello

hello: objets.cmo

#hello: amodule.cmo bmodule.cmo
#	ocamlc -o hello amodule.cmo bmodule.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

objets.cmo: objets.ml objets.cmi