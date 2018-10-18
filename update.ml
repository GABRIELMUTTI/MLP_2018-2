open Config;;
open Utilities;;


(* COLLISION*)
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
  if state.bullet_on then
    let (hit, final_enemies) = aux_loop state.enemies in
    if hit then
      { state with bullet_on = false; enemies = final_enemies; bullet_delay = 0.0 }
    else
      state
  else
    state
;;



let clear_bullet_out_of_screen state =
  let (bx, by) = state.bullet in
  if by > snd _screen_size then
    { state with bullet_on = false; bullet_delay = 0.0 }
  else
    state
;;

let clear_enemy_bullets_out_of_screen state =
  let rec aux_loop bullets =
    match bullets with
    | [] -> []
    | (ex, ey) :: tl ->
       if ey < 0 then
         aux_loop tl
       else
         List.cons (ex, ey) (aux_loop tl) in
  let bullets' = aux_loop state.enemy_bullets in
  { state with enemy_bullets = bullets' }
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

let update_enemy_bullets state dt =
  if state.enemy_bullet_delay > _bullet_speed then
    { state with
      enemy_bullets = List.map
                        (fun (bx, by) -> (bx, by - _bullet_step_distance))
                        state.enemy_bullets;
      enemy_bullet_delay = 0.0;
    }
  else
    { state with enemy_bullet_delay = state.enemy_bullet_delay +. dt }
  
;;

let select_random_enemy enemies =
  if List.length enemies > 1 then
    (let random_index = Random.int ((List.length enemies) - 1) in
     List.nth enemies random_index)
  else
    List.nth enemies 0

;;

let fire_enemy_bullet state dt =
  if state.enemy_fire_delay > _enemy_firerate then
    let enemy = select_random_enemy state.enemies in
    let bullets' = List.cons enemy state.enemy_bullets in
    { state with enemy_bullets = bullets'; enemy_fire_delay = 0.0 }
  else
    { state with enemy_fire_delay = state.enemy_fire_delay +. dt }
;;    

let check_game_over state =
  if ((snd (enemy_far_down state.enemies)) <= _game_over_line )then 
    {state with game_over = true}
    else state
;;

let update_state state dt =
  let state1' = if List.length state.enemies > 0 then
                        (let enemy_state1' = if state.enemy_direction then update_enemies_right state dt 
                                            else update_enemies_left state dt in
                        let enemy_state2' = fire_enemy_bullet enemy_state1' dt in
                        enemy_state2')
                else
                  state in
                        
                        
  let state2' = if state.bullet_on then update_bullet state1' dt else state1' in
  let state3' = check_player_boundaries state2' in
  let state4' = check_bullet_enemy_collision state3' in
  let state5' = clear_bullet_out_of_screen state4' in
  let state6' = clear_enemy_bullets_out_of_screen state5' in
  let state7' = update_enemy_bullets state6' dt in
  let state8' = check_game_over state7' in
  state8'
;;

let fire_bullet state =
  if state.bullet_on then
    state
  else
    let bullet =( ((fst state.player) + 22 ), ((snd state.player) + (snd _player_size))) in
    { state with bullet = bullet;bullet_on = true }
  
  
;;
