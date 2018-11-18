type coords =
  {
    x : int;
    y : int;
  }

(* Classe  base. *)
class virtual game_object (position : coords)
                (size : int * int)
                (speed : float)
                (step_distance : int)
                (object_type : string) = object(self)
  val mutable position = position
  val mutable size = size
  val mutable speed = speed
  val mutable step_distance = step_distance                    
  val mutable object_type = object_type
                            
  method virtual update : float -> unit
  method virtual draw : unit -> unit
  method virtual onCollision : game_object -> int -> unit
                    
  method getPosition = position
  method getSize = size
  method getSpeed = speed
  method getType = object_type

                  
  method setPosition new_pos =
    let {x : int; y : int} = new_pos in
    if x >= 0 && y >= 0 then
      position <- new_pos

  method checkCollision (other : game_object) =
    let {x : int; y : int} = other#getPosition in
    let (sx, sy) = other#getSize in
                             
    if position.x + (fst size) >= x &&
         position.x + (fst size) <= x + sx &&
           position.y + (snd size) >= y &&
             position.y + (snd size) <= y + sy then
      true
    else
      false
                    

end
