open GameObject;;
open Config;;
open Graphics;;


(* Classe das naves. *)       
class virtual ship position = object(self)
  inherit game_object position         
end