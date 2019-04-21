all:
	ocamlc -c a1.mli
	ocamlc -c a1.ml
	ocamllex a2.mll       # generates a2.ml
	ocamlyacc a3.mly     # generates a3.ml and a3.mli
	ocamlc -c a3.mli
	ocamlc -c a2.ml
	ocamlc -c a3.ml
	ocamlc -c calculator_krivine.ml
	ocamlc -c calculator_secd.ml
	ocamlc -o calculate_krivine a1.cmo a2.cmo a3.cmo calculator_krivine.cmo
	ocamlc -o calculate_secd a1.cmo a2.cmo a3.cmo calculator_secd.cmo

clean:
	rm calculate_krivine calculate_secd *.cmo *.cmi  a2.ml a3.ml a3.mli
