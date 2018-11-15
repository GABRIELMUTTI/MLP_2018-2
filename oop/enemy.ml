open Ship;;
open Config;;
open Graphics;;

(* Classe dos inimigos. *)
class enemy position = object(self)
  inherit ship position
  val speed = _enemy_speed
  val size = _enemy_size
  val step_distance = _enemy_step_distance
  val downstep_distance = _enemy_downstep_distance
  val mutable direction = 0 (* Right = 0 ; Left = 1 ; Down = 2 *)
  val mutable delay = 0.0
  val mutable down = false (*used for updating down only once*)

  method setDown b = down  <- b
  method getDown = down
  method getDirection = direction
  method setDirection n = direction <- n
  method private updateRight dt = 
    if delay > speed then
      begin position <- {position with x = position.x + step_distance};
            delay <- 0.0 end
    else 
      delay <- delay +. dt

  method private updateLeft dt = 
    if delay > speed then
      begin position <- {position with x = position.x - step_distance};
            delay <-0.0 end
    else 
      delay <- delay +. dt

  method private updateDown dt = 
    if delay > speed then
      begin position <- {position with y = position.y - downstep_distance};
            delay <- 0.0; self#setDown true end
    else 
      delay <- delay +. dt; 

  method update dt = 
    match direction with
      | 0 -> self#updateRight dt
      | 1 -> self#updateLeft dt
      | 2 -> self#updateDown dt
      | _ -> ()

  method draw = 
    set_color green;
    fill_rect position.x position.y (fst size) (snd size);
    set_color black;
    fill_rect (position.x+5) (position.y+15) 6 6;
    fill_rect (position.x+17) (position.y+15) 6 6;
    fill_rect (position.x) position.y 2 6;
    fill_rect (position.x+25) position.y 2 6;
    fill_rect (position.x+7) position.y 4 6;
    fill_rect (position.x+16) position.y 4 6
end