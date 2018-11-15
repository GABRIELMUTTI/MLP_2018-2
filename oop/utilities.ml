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
  let list = List.sort (fun a b -> compare a#getPosition.x b#getPosition.x) enemy_list in
    List.hd (List.rev list)
;;

let enemy_far_left enemy_list =
  let list = List.sort (fun a b -> compare a#getPosition.x b#getPosition.x) enemy_list in
    List.hd list
;;

let changeDirection (enemy_list: Enemy.enemy list) =
  match (List.hd enemy_list)#getDirection with
    | 0 -> if (enemy_far_right enemy_list)#getPosition.x >= (snd _enemy_area_border) then
              List.iter (fun x -> x#setDirection 2) enemy_list else ()

    | 1 -> if (enemy_far_left enemy_list)#getPosition.x <= (fst _enemy_area_border) then
              List.iter (fun x -> x#setDirection 2) enemy_list else ()

    | 2 -> if  (List.hd enemy_list)#getDown then
             begin
              if (enemy_far_right enemy_list)#getPosition.x >= (snd _enemy_area_border) then
                begin List.iter (fun x -> x#setDirection 1) enemy_list;
                      List.iter (fun x -> x#setDown false) enemy_list end
              else 
                begin List.iter (fun x -> x#setDirection 0) enemy_list;
                      List.iter (fun x -> x#setDown false) enemy_list end
              end
           else
            ()

    |_ -> ()

let get_time_now () = 
  Unix.gettimeofday ()
;;
