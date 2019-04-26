export OCAMLFIND_CONF=/home/ariana/.opam/4.06.1/lib/findlib.conf

GEN_JS_API_DIR=/home/ariana/.opam/4.06.1/lib/gen_js_api
JS_OF_OCAML_DIR=/home/ariana/.opam/4.06.1/bin
JS_OF_OCAML_LIBDIR=/home/ariana/.opam/4.06.1/lib/js_of_ocaml-compiler

CAMLC=ocamlc
COMPFLAGS=-I $(GEN_JS_API_DIR)

GEN_JS_API=ocamlrun $(GEN_JS_API_DIR)/gen_js_api
OJSFLAGS=gen_js_api.cma

JS_OF_OCAML=$(JS_OF_OCAML_DIR)/js_of_ocaml
JS_OF_OCAML_FLAGS=--no-runtime $(JS_OF_OCAML_LIBDIR)/runtime.js $(GEN_JS_API_DIR)/ojs_runtime.js

ENGINE_OBJS=port.cmo moves.cmo solver.cmo maps.cmo formes.cmo
JS_DEPS=dom.cmi
JS_OBJS=dom.cmo $(ENGINE_OBJS)

all : orush ocaml-formes.js

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

dom.ml: dom.mli
	$(GEN_JS_API) dom.mli

ocaml-formes.byte: $(JS_DEPS) $(JS_OBJS)
	$(CAMLC) $(COMPFLAGS) -no-check-prims -o $@ $(OJSFLAGS) $(JS_OBJS)

ocaml-formes.js: ocaml-formes.byte
	$(JS_OF_OCAML) $(JS_OF_OCAML_FLAGS) $^

orush: port.cmo moves.cmo solver.cmo orush.cmo
	$(CAMLC) port.cmo moves.cmo solver.cmo orush.cmo -o orush

port.cmo: port.ml
	$(CAMLC) -c port.ml

maps.cmo: maps.ml port.cmo
	$(CAMLC) -c maps.ml

moves.cmo: moves.ml port.cmo
	$(CAMLC) -c moves.ml

solver.cmo: solver.ml port.cmo moves.cmo
	$(CAMLC) -c solver.ml

orush.cmo: orush.ml port.cmo moves.cmo solver.cmo
	$(CAMLC) -c orush.ml

clean:
	rm -rf *.cmo *.cmi *.cmx *.cma *.o *.byte
	rm -f dom.ml ocaml-formes.js orush