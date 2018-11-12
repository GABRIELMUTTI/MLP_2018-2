type coords =
  {
    x : int;
    y : int;
  }

(* Classe  base. *)
class virtual game_object position = object(self)
  val mutable position : coords = position;
                                  
  method virtual update : unit
               
  method get_position =
    position
end

(* Classe dos tiros. *)
class bullet position = object(self)
  inherit game_object position

  method update = ()
end

(* Classe das naves. *)       
class virtual ship position = object(self)
  inherit game_object position         
end

(* Classe to jogador. *)
class player position = object(self)
  inherit ship position

  method update = ()
end

(* Classe dos inimigos. *)
class enemy position = object(self)
  inherit ship position

  method update = ()
end
                            
let  main () =
  Printf.printf "x=%d, y=%d\n" 0 0
  
  
let _ = main ()
          





          
