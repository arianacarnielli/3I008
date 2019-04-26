# Modifiler la ligne suivante pour qu'elle contienne le chemin vers la version
# 4.06.1 d'opam. Ne pas utiliser le ~ pour le dossier personnel (problème de
# compilation avec js_of_ocaml, qui ne retrouve pas le findlib.conf si son
# chemin contient un ~).
OPAM_PATH=/home/ariana/.opam/4.06.1

export OCAMLFIND_CONF=$(OPAM_PATH)/lib/findlib.conf

GEN_JS_API_DIR=$(OPAM_PATH)/lib/gen_js_api
JS_OF_OCAML_DIR=$(OPAM_PATH)/bin
JS_OF_OCAML_LIBDIR=$(OPAM_PATH)/lib/js_of_ocaml-compiler

CAMLC=ocamlc
COMPFLAGS=-I $(GEN_JS_API_DIR)

GEN_JS_API=ocamlrun $(GEN_JS_API_DIR)/gen_js_api
OJSFLAGS=gen_js_api.cma

JS_OF_OCAML=$(JS_OF_OCAML_DIR)/js_of_ocaml
JS_OF_OCAML_FLAGS=--no-runtime $(JS_OF_OCAML_LIBDIR)/runtime.js $(GEN_JS_API_DIR)/ojs_runtime.js

ENGINE_OBJS=port.cmo moves.cmo solver.cmo maps.cmo orush_js.cmo
JS_DEPS=dom.cmi
JS_OBJS=dom.cmo $(ENGINE_OBJS)

all : orush ocaml-orush.js

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

dom.ml: dom.mli
	$(GEN_JS_API) dom.mli

ocaml-orush.byte: $(JS_DEPS) $(JS_OBJS)
	$(CAMLC) $(COMPFLAGS) -no-check-prims -o $@ $(OJSFLAGS) $(JS_OBJS)

ocaml-orush.js: ocaml-orush.byte
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
	rm -f dom.ml ocaml-orush.js orush
