compile: SpaceInvaders.ml types.ml config.ml
	ocamlfind ocamlopt -I +threads -o SpaceInvaders graphics.cmxa unix.cmxa threads.cmxa  config.ml types.ml SpaceInvaders.ml

run: SpaceInvaders
	./SpaceInvaders -I +threads


clean: 
	rm -rf *.o *~ *.cmx *.cmi


