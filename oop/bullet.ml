open GameObject;;
open Config;;
open Graphics;;

(* Classe dos tiros. *)
class  bullet position owner = object(self)
  inherit game_object position
  val speed = _bullet_speed
  val size = _bullet_size
  val step_distance = _bullet_step_distance
  val mutable delay = 0.0
  
  val mutable on = false
  method setOn b = on <- b 
  method getOn = on
  

  method update dt = 
  if owner == 1 || (owner == 0 && on) then
    if delay > speed then
      begin position <-{position with y = if owner == 0 then
                                            position.y + step_distance
                                          else
                                            position.y - step_distance };
            delay <- 0.0 end
    else
      delay <- delay +.dt 
  else ()          

  method draw = 
  if owner == 1 || (owner == 0 && on) then
    begin 
    if owner == 0 then set_color (rgb 125 125 125)
      else set_color red
    ;
    fill_rect position.x position.y (fst size) (snd size)
    end
  else
    ()
 


end
