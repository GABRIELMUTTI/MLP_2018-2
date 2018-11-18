open GameObject;;
open Config;;
open Graphics;;

(* Classe dos tiros. *)
class bullet position size speed step_distance owner = object(self)
  inherit game_object position size speed step_distance "bullet"
  val mutable delay = 0.0
  
  val mutable on = false
  method setOn b = on <- b 
  method getOn = on

  method getOwner = owner

  method update dt = 
  (if owner == 1 || (owner == 0 && on) then
    if delay > speed then
      begin position <-{position with y = if owner == 0 then
                                            position.y + step_distance
                                          else
                                            position.y - step_distance };
            delay <- 0.0 end
    else
      delay <- delay +.dt 
   else ());

  let collided = false in
  let i = ref 0 in
  while not collided && !i < (List.length !_objects) do
    let obj = !(List.nth !_objects !i) in
    if obj != (self :> game_object) then
      (if self#checkCollision obj then
         (obj#onCollision (self :> game_object) owner;
          self#setOn false););
       
    i := !i + 1
  done; ()
  

  method draw () = 
  if owner == 1 || (owner == 0 && on) then
    begin 
    if owner == 0 then set_color (rgb 125 125 125)
      else set_color red
    ;
    fill_rect position.x position.y (fst size) (snd size)
    end
  else
    ()

  method onCollision obj owner = ()
 


end
