open Config;;

(*** UTILITIES FUNCTIONS ***)
let rec build_enemies i k = 
  if i = _enemies_rows
    then if k = _enemies_lines
          then []
          else (build_enemies 0 (k + 1))
    else ((fst _first_enemy_pos) + (fst _space_between_enemies)*i,
          (snd _first_enemy_pos)-(snd _space_between_enemies)*k)::build_enemies (i+1) k
;;

let get_time_now () = 
  Unix.gettimeofday ()
;;

let enemy_far_right enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd (List.rev list)
;;

let enemy_far_left enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd list
;;

let enemy_far_down enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare y y2) enemy_list in
    List.hd list
;;
