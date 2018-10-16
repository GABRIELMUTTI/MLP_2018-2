open Graphics;;
open Printf;;
open StdLabels;;

type game_state =
{
  player : int * int;
  enemies : (int * int) list;
  bullets : (int * int) list;
}

let enemys_line = 4;;
let enemys_row = 8;;
open_graph " 900x600";;
let rec build_enemys r l = 
  if r < 0 
    then if l = 0 
          then []
          else (build_enemys enemys_row (l-1))
    else (70*r,550-50*l)::build_enemys (r-1) l
;;

let get_time_now () = 
  Unix.time ()
;;

let draw_world state =
  clear_graph ();
  fill_rect (fst state.player) (snd state.player) 100 100;
  List.iter (fun (x, y) -> fill_rect x y 25 25) state.enemies;
;;

let enemy_far_right enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd (List.rev list)
;;


let update_enemies enemies =
  List.map (if fst (enemy_far_right enemies) >= 800 then
              (fun (x,y) -> ((x-200) , y-70)) 
            else 
              (fun (x,y) ->((x+50), y))) enemies
;;

let update_bullets bullets =
    List.map (fun (x, y) -> (x, y + 5)) bullets
;;

let update_state state =
  let enemies' = update_enemies state.enemies in
  let bullets' = update_bullets state.bullets in
  { state with enemies = enemies'; bullets = bullets' }
;;

let rec handler state old_time =
  draw_world state;

  let new_time = get_time_now () in
  let state' = if ((new_time -. old_time) > 0.5) then
                 update_state state
               else
                 state in
    
  let event = Graphics.wait_next_event [ Graphics.Poll ] in
    if event.Graphics.keypressed then
      match (read_key ()) with
      |'a' -> handler { state' with player = ((fst state'.player) - 20, snd state'.player) } new_time
      |'d' -> handler { state' with player = ((fst state'.player) + 20, snd state'.player) } new_time
      | _ -> handler state' new_time
    else
      handler state' new_time
;;

loop_at_exit [Key_pressed ; Button_down]
   (fun event ->
      handler { 
          player = (300,100);
          enemies = build_enemys enemys_row enemys_line;
          bullets = [ (150, 150) ]
        } 0.0)
;;
