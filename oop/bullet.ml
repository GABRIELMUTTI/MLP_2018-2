open GameObject;;
open Config;;
open Graphics;;

(* Classe dos tiros. *)
class bullet position = object(self)
  inherit game_object position
  val speed = _bullet_speed
  val size = _bullet_size
  val step_distance = _bullet_step_distance
  method update = ()
  method draw = ()
end