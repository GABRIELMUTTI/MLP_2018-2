open Config;;

(* Classe  base. *)
class virtual game_object position = object(self)
  val mutable position : coords = position; 
  val virtual speed : float   
  val virtual size : int * int  
  val virtual step_distance : int                      
  method virtual update: float -> unit
  method virtual draw : unit
               
  method getPosition =
    position

  method setPosition coord = position <- coord;
end