open Graphics;;
open Printf;;
open StdLabels;;
open Thread;;

type game_state =
{
  player : int * int;
  enemies : (int * int) list;
  bullets : (int * int) list;
  enemy_delay : float;
  bullet_delay : float;
  enemy_speed : float;
  bullet_speed : float;
}

let enemies_line = 4;;
let enemies_row = 8;;
open_graph " 900x600";;

let rec build_enemies r l = 
  if r < 0 
    then if l = 0 
          then []
          else (build_enemies enemies_row (l-1))
    else (70*r,550-50*l)::build_enemies (r-1) l
;;

let get_time_now () = 
  Unix.time ()
;;


let draw_world state =
  auto_synchronize false;
  clear_graph ();
  fill_rect (fst state.player) (snd state.player) 100 100;
  List.iter (fun (x, y) -> fill_rect x y 25 25) state.enemies;
  List.iter (fun (x, y) -> fill_rect x y 5 10) state.bullets;
  synchronize ();
;;

let enemy_far_right enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd (List.rev list)
;;

let update_enemies state dt =
  if state.enemy_delay > state.enemy_speed then
    { state with enemies = List.map (if fst (enemy_far_right state.enemies) >= 800 then
                                      (fun (x,y) -> ((x-200) , y-70)) 
                                    else 
                                      (fun (x,y) ->((x+50), y))) state.enemies;
                 enemy_delay = 0.0 }
  else
    { state with enemy_delay = state.enemy_delay +. dt }

let update_bullets state dt =
  if state.bullet_delay > state.bullet_speed then
    { state with bullets = List.map (fun (x, y) -> (x, y + 10)) state.bullets;
                 bullet_delay = 0.0 }
  else
    { state with bullet_delay = state.bullet_delay +. dt }
    
;;

let update_state state dt =
  let state' = update_enemies state dt in
  let state' = update_bullets state' dt in
  state'
;;

let fire_bullet state =
  let bullet = state.player in
  { state with bullets = List.append state.bullets [bullet] }

let rec handler state old_time =
  draw_world state;

  let new_time = get_time_now () in
  let delta_time = 0.0001 in
  let new_state = update_state state delta_time in

  let event = Graphics.wait_next_event [ Graphics.Poll ] in
    if event.Graphics.keypressed then
      match (read_key ()) with
      |'a' -> handler { new_state with player = ((fst new_state.player) - 20, snd new_state.player) } new_time
      |'d' -> handler { new_state with player = ((fst new_state.player) + 20, snd new_state.player) } new_time
      |' ' -> handler (fire_bullet new_state) new_time
      | _ -> handler new_state new_time
    else
      handler new_state new_time
;;

loop_at_exit [Key_pressed ; Button_down]
   (fun event ->
      handler { 
          player = (300,100);
          enemies = build_enemies enemies_row enemies_line;
          bullets = [ (150, 150) ];
          enemy_delay = 0.0;
          bullet_delay = 0.0;
          enemy_speed = 0.5;
          bullet_speed = 0.01;
        } 0.0)
;;
