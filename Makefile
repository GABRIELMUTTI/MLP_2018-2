SpaceIncaders: SpaceInvaders.ml
	ocamlfind ocamlc -o $@ unix.cma  -thread threads.cma graphics.cma $^ 
