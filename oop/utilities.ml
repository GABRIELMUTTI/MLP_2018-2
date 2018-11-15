open Config;;
open Enemy;;

let  build_enemies  () =
  let enemies = ref [] in
  for i = 0 to _enemies_rows do
    for k = 0 to _enemies_lines do
        enemies := !enemies@[new enemy {x =(fst _first_enemy_pos) + (fst _space_between_enemies)*i;
                            y = (snd _first_enemy_pos)-(snd _space_between_enemies)*k}
        ]  
      done
        
  done;
  !enemies
;;

let enemy_far_right enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd (List.rev list)
;;

let enemy_far_left enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd list
;;


let get_time_now () = 
  Unix.gettimeofday ()
;;
