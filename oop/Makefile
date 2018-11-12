SpaceInvaders: types.ml
	ocamlfind ocamlopt -I +threads -o SpaceInvaders graphics.cmxa unix.cmxa threads.cmxa types.ml

run: SpaceInvaders
	./SpaceInvaders -I +threads

git:
	git add Makefile
	git add SpaceInvaders.ml
	git add update.ml
	git add drawing.ml
	git add utilities.ml
	git add config.ml
