open Graphics;;
open Printf;;
open StdLabels;;
open Thread;;

(*** GAME CONTROL AND DESIGN "CONSTANTS" ***)

    (* player *)
let _initial_player_pos = (300,25);;
let _player_size = (50,25);;
let _player_step_distance = 20;;

    (* bullet *)
let _bullet_speed = 0.001;;
let _bullet_size = (5,10);;
let _bullet_step_distance = 10;;

    (* enemy *)
let _enemies_lines = 4;;
let _enemies_rows = 8;;
let _enemy_speed = 0.01;;
let _enemy_size = (25,25);;
let _enemy_step_distance = 20;;
let _enemy_downstep_distance = 30;;
let _enemy_area_border = (20,870) ;;
let _first_enemy_pos = (20, 550);;
let _space_between_enemies = ((fst _enemy_size)+45, (snd _enemy_size)+25);;


(*** OPEN WINDOW***)
open_graph " 900x600";;


(*** TYPES ***)
type game_state =
{
  player : int * int;
  enemies : (int * int) list;
  bullets : (int * int) list;
  enemy_delay : float;
  bullet_delay : float;
  enemy_speed : float;
  bullet_speed : float;
  enemy_direction : bool;
}


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
  Unix.time ()
;;

let enemy_far_right enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd (List.rev list)
;;

let enemy_far_left enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd list
;;



(*** DRAWING FUNCTIONS***)

let draw_enemy (x,y) =
  fill_rect x y (fst _enemy_size) (snd _enemy_size);
;;

let rec draw_all_enemies enemy_list =
  List.iter (fun i -> Thread.join i)
    (List.map (fun e -> Thread.create draw_enemy e ) enemy_list)   
;;

let draw_world state =
  auto_synchronize false;
  clear_graph ();
  fill_rect (fst state.player) (snd state.player) (fst _player_size) (snd _player_size);
  draw_all_enemies state.enemies;
  List.iter (fun (x, y) -> fill_rect x y (fst _bullet_size) (snd _bullet_size)) state.bullets;
  
  synchronize ();
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

let update_bullets state dt =
  if state.bullet_delay > state.bullet_speed then
    { state with bullets = List.map (fun (x, y) -> (x, y + _bullet_step_distance)) state.bullets;
                 bullet_delay = 0.0 }
  else
    { state with bullet_delay = state.bullet_delay +. dt }
    
;;

let update_state state dt =
  let state' = if state.enemy_direction then update_enemies_right state dt 
               else update_enemies_left state dt in
  let state' = update_bullets state' dt in
  state'
;;

let fire_bullet state =
  let bullet = state.player in
  { state with bullets = List.append state.bullets [bullet] }
;;


(*** MAIN FUNCTIONS ***)
let rec mainloop state old_time =
  draw_world state;

  let new_time = get_time_now () in
  let delta_time = 0.0001 in
  let new_state = update_state state delta_time in

  let event = Graphics.wait_next_event [ Graphics.Poll ] in
    if event.Graphics.keypressed then
      match (read_key ()) with
      |'a' -> mainloop { new_state with player = ((fst new_state.player) - _player_step_distance, snd new_state.player) } new_time
      |'d' -> mainloop { new_state with player = ((fst new_state.player) + _player_step_distance, snd new_state.player) } new_time
      |' ' -> mainloop (fire_bullet new_state) new_time
      | _ -> mainloop new_state new_time
    else
      mainloop new_state new_time
;;

loop_at_exit [Key_pressed ; Button_down]
   (fun event ->
      mainloop { 
          player = _initial_player_pos;
          enemies = build_enemies 0 0;
          bullets = [ (150, 150) ];
          enemy_delay = 0.0;
          bullet_delay = 0.0;
          enemy_speed = _enemy_speed;
          bullet_speed = _bullet_speed;
          enemy_direction = true;
        } 0.0)
;;
