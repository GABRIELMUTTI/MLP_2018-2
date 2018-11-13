open Config;;
open Graphics;;

type coords =
  {
    x : int;
    y : int;
  }

(* Classe  base. *)
class virtual game_object position = object(self)
  val mutable position : coords = position; 
  val virtual speed : float   
  val virtual size : int * int  
  val virtual step_distance : int                      
  method virtual update : unit
  method virtual draw : unit
               
  method get_position =
    position
end

(* Classe dos tiros. *)
class bullet position = object(self)
  inherit game_object position
  val speed = _bullet_speed
  val size = _bullet_size
  val step_distance = _bullet_step_distance
  method update = ()
  method draw = ()
end

(* Classe das naves. *)       
class virtual ship position = object(self)
  inherit game_object position         
end

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
    set_color black;
    fill_rect position.x position.y (fst size) (snd size);
    set_color white;
    fill_rect position.x (position.y+12) 15 13;
    fill_rect (position.x + ((fst size) - 15)) (position.y+12) 15 13;
end

(* Classe dos inimigos. *)
class enemy position = object(self)
  inherit ship position
  val speed = _enemy_speed
  val size = _enemy_size
  val step_distance = _enemy_step_distance
  method update  = ()
  method draw = ()
end
                            

          





          
