open Ship;;
open Config;;
open Graphics;;

(* Classe do jogador. *)
class player position = object(self)
  inherit ship position
  val speed = 0.0   (*not used*)
  val size = _player_size
  val step_distance = _player_step_distance

  val mutable key  = '0'
  method setKey keyPressed = 
    key <- keyPressed

  method getKey = key
  method update  = 
    match key with
    |'a' -> position <- {position with x = position.x - step_distance }
    |'d' -> position <- {position with x = position.x + step_distance }
    |_ -> position <- position


  method draw =  
    set_color white;
    fill_rect position.x position.y (fst size) (snd size);
    set_color black;
    fill_rect position.x (position.y+12) 15 13;
    fill_rect (position.x + ((fst size) - 15)) (position.y+12) 15 13;
end