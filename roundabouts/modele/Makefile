all: hello

hello: amodule.cmo bmodule.cmo
	ocamlc -o hello amodule.cmo bmodule.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

amodule.cmo: amodule.cmi amodule.ml