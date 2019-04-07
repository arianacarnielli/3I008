all: orush

orush: port.cmo moves.cmo solver.cmo orush.cmo
	ocamlc port.cmo moves.cmo solver.cmo orush.cmo -o orush

port.cmo: port.ml
	ocamlc -c port.ml

moves.cmo: moves.ml port.cmo
	ocamlc -c moves.ml

solver.cmo: solver.ml port.cmo moves.cmo
	ocamlc -c solver.ml

orush.cmo: orush.ml port.cmo moves.cmo solver.cmo
	ocamlc -c orush.ml

clean:
	rm -f *.cmo *.cmi orush
