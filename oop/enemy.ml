open Ship;;
open Config;;
open Graphics;;

(* Classe dos inimigos. *)
class enemy position = object(self)
  inherit ship position
  val speed = _enemy_speed
  val size = _enemy_size
  val step_distance = _enemy_step_distance
  method update  = ()
  method draw = ()
end