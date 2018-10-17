open Config;;
open Utilities;;


let check_collision (bx, by) (ex, ey) =
  if bx + (fst _bullet_size) >= ex &&
       bx <= ex + (fst _enemy_size) &&
         by + (snd _bullet_size) >= ey &&
           by <= ey + (snd _enemy_size) then
    true
  else
    false
;;

let check_bullet_enemy_collision state =
  let rec aux_loop enemies =
    match enemies with
    | [] -> (false, [])
    | hd :: tl ->
       if check_collision state.bullet hd then
         (true, tl)
       else
         let (hit', enemies') = aux_loop tl in
         (hit', List.cons hd enemies')
  in
  let (hit, final_enemies) = aux_loop state.enemies in
  if hit then
    { state with bullet_on = false; enemies = final_enemies; bullet_delay = 0.0 }
  else
    state
;;            
    


(*** UPDATE FUNCTIONS ***)
let update_enemies_right state dt =
  if state.enemy_delay > state.enemy_speed then
    if (fst (enemy_far_right state.enemies) + (fst _enemy_size)) >= (snd _enemy_area_border) then
      { state with
        enemies = List.map (fun (x,y) ->(x, (y - _enemy_downstep_distance))) state.enemies;
        enemy_direction = false;
        enemy_delay = 0.0}
    else
      { state with 
        enemies = List.map (fun (x,y) ->((x + _enemy_step_distance), y)) state.enemies;
        enemy_delay = 0.0 }

  else
    { state with enemy_delay = state.enemy_delay +. dt }
;;

let update_enemies_left state dt =
  if state.enemy_delay > state.enemy_speed then
    if (fst (enemy_far_left state.enemies)) <= (fst _enemy_area_border) then
      { state with
        enemies = List.map (fun (x,y) ->(x, (y - _enemy_downstep_distance))) state.enemies;
        enemy_direction = true;
        enemy_delay = 0.0}
    else
      { state with 
        enemies = List.map (fun (x,y) ->((x - _enemy_step_distance), y)) state.enemies;
        enemy_delay = 0.0 }

  else
    { state with enemy_delay = state.enemy_delay +. dt }
;;

let update_bullet state dt =
  if state.bullet_delay > state.bullet_speed then
    { state with bullet = ((fst state.bullet), (snd state.bullet) + _bullet_step_distance);
                 bullet_delay = 0.0 }
  else
    { state with bullet_delay = state.bullet_delay +. dt }
    
;;
let check_player_boundaries state =
  let player_pos = state.player in
    if (fst player_pos) < (fst _player_boundaries) then
      { state with player = ( (fst _player_boundaries), (snd player_pos))}
    else if ((fst player_pos)+(fst _player_size)) > (snd _player_boundaries) then
            { state with player = ( ((snd _player_boundaries)-(fst _player_size)), (snd player_pos))}
         else
            state
;;


let update_state state dt =
  let state' = if state.enemy_direction then update_enemies_right state dt 
               else update_enemies_left state dt in
  let state2' = if state.bullet_on then update_bullet state' dt else state' in
  let state3' = check_player_boundaries state2' in
  let state4' = check_bullet_enemy_collision state3' in
  state4'
;;

let fire_bullet state =
  if state.bullet_on then
    state
  else
    let bullet = state.player in
    { state with bullet = bullet;bullet_on = true }
  
  
;;
