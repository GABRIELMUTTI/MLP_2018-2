open Ship;;
open Config;;
open Graphics;;
open Bullet;;

(* Classe do jogador. *)
class player position size speed step_distance = object(self)
  inherit ship position _player_size speed step_distance "player"
  val mutable life = _player_life
  val mutable key  = '0'
  method setKey keyPressed = 
    key <- keyPressed
  method getKey = key
  method getLife = life
  

  method hit = life <- life -1

  method private updatePosition =
    if position.x < (fst _player_boundaries) then 
      {position with x = (fst _player_boundaries)}
    else if (position.x + (fst _player_size)) > (snd _player_boundaries) then
      { position with x = ((snd _player_boundaries)-(fst _player_size))}
    else
      match key with
      |'a' -> {position with x = position.x - step_distance }
      |'d' -> {position with x = position.x + step_distance }
      |_ -> position

  method update dt = 
     position <- self#updatePosition; 
     key <- '0' 
    

  method private draw_life life = 
    set_color white;
    moveto 830 5;
    draw_string "LIFES : " ;
    draw_string (string_of_int life)  
    
  method draw =  
    set_color white;
    fill_rect position.x position.y (fst size) (snd size);
    set_color black;
    fill_rect position.x (position.y+12) 15 13;
    fill_rect (position.x + ((fst size) - 15)) (position.y+12) 15 13;
    self#draw_life life

  method onCollision obj owner =
    if owner == 1 then
      (self#hit;
     _objects := List.filter (fun x -> !x != obj) !_objects)
end
