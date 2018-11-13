SpaceInvaders: config.ml utilities.ml drawing.ml update.ml SpaceInvaders.ml
	ocamlfind ocamlopt -I +threads -o SpaceInvaders  graphics.cmxa unix.cmxa threads.cmxa config.ml utilities.ml drawing.ml update.ml SpaceInvaders.ml 

run: SpaceInvaders
	./SpaceInvaders -I +threads

git:
	git add Makefile
	git add SpaceInvaders.ml
	git add update.ml
	git add drawing.ml
	git add utilities.ml
	git add config.ml


clean: 
	rm -rf *.o *~ *.cmx *.cmi