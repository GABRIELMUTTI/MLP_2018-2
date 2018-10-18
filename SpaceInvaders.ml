open Graphics;;
open Update;;
open Utilities;;
open Drawing;;
open Config;;
open Thread;;













(*** MAIN FUNCTIONS ***)
let  initial_screen () = 
  draw_initial_screen ();
  ignore(Graphics.wait_next_event [ Key_pressed ]);
;;

let rec game_over_screen () =

  draw_game_over_screen ();
  ignore ( Graphics.wait_next_event [ Key_pressed ]);
  match (read_key ()) with
  |'\027' -> ()
  |_ -> game_over_screen ()
;;

let rec mainloop state old_time =

  let new_time = get_time_now () in
  let delta_time = new_time -. old_time in
  let new_state = update_state state delta_time in

  draw_world state;
  if state.game_over then state
  else
    let event = Graphics.wait_next_event [ Graphics.Poll ] in
      if event.Graphics.keypressed then
        match (read_key ()) with
        |'a' -> mainloop { new_state with player = ((fst new_state.player) - _player_step_distance, snd new_state.player) } new_time
        |'d' -> mainloop { new_state with player = ((fst new_state.player) + _player_step_distance, snd new_state.player) } new_time
        |'\027' ->  state; (* ESC -> exit*)
        |' ' -> mainloop (fire_bullet new_state) new_time
        | _ -> mainloop new_state new_time
      else
        mainloop new_state new_time
;;

let  main () = 

  open_graph " 900x600";
  initial_screen ();
  let state = 
    mainloop { 
      player = _initial_player_pos;
      enemies = build_enemies 0 0;
      bullet = (0,0);
      bullet_on = false;
      enemy_delay = 0.0;
      bullet_delay = 0.0;
      enemy_speed = _enemy_speed;
      bullet_speed = _bullet_speed;
      enemy_direction = true;
      enemy_bullets = [];
      enemy_bullet_delay = 0.0;
      enemy_fire_delay = 0.0;
      game_over = false;
      
    } 0.0 in

  if state.game_over then game_over_screen ();
  close_graph ();
;;

let _ = main ();;


