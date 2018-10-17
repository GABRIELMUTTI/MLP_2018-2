open Graphics;;
open Update;;
open Utilities;;
open Drawing;;
open Config;;
open Thread;;
(*** OPEN WINDOW***)
open_graph " 900x600";;











(*** MAIN FUNCTIONS ***)
let rec mainloop state old_time =

  let new_time = get_time_now () in
  let delta_time = new_time -. old_time in
  let new_state = update_state state delta_time in

  draw_world state;

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
