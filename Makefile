SpaceInvaders: SpaceInvaders.ml
	ocamlfind ocamlc -o $@ unix.cma  -thread threads.cma graphics.cma $^ 

run: SpaceInvaders
	./SpaceInvaders -I +threads