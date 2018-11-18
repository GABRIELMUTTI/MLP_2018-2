open GameObject;;
open Config;;
open Graphics;;


(* Classe das naves. *)       
class virtual ship position size speed step_distance object_type = object(self)
  inherit game_object position size speed step_distance object_type
end
