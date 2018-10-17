open Config;;
open Utilities;;





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

let update_bullets state dt =
  if state.bullet_delay > state.bullet_speed then
    { state with bullets = List.map (fun (x, y) -> (x, y + _bullet_step_distance)) state.bullets;
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
  let state2' = update_bullets state' dt in
  let state3' = check_player_boundaries state2' in
  state3'
;;

let fire_bullet state =
  let bullet = state.player in
  { state with bullets = List.append state.bullets [bullet] }
;;