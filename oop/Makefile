compile: config.ml  gameObject.ml bullet.ml ship.ml player.ml enemy.ml screen.ml utilities.ml SpaceInvaders.ml 
	ocamlfind ocamlopt -I +threads -o SpaceInvaders graphics.cmxa unix.cmxa threads.cmxa  $^

run: SpaceInvaders
	./SpaceInvaders -I +threads


clean: 
	rm -rf *.o *~ *.cmx *.cmi


