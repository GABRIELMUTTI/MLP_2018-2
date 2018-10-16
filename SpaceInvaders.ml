open Graphics;;
open Printf;;
open StdLabels;;
open Thread;;

type game_state =
{
  player : int * int;
  enemies : (int * int) list;
  bullets : (int * int) list;
}

let enemies_line = 4;;
let enemies_row = 8;;
let speed_enemy = 0.5;;
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

let draw_enemy (x,y) =
  fill_rect x y 25 25;
;;

let rec draw_all_enemies enemy_list i =
  if i < (List.length enemy_list) then
    (Thread.create draw_enemy (List.nth enemy_list i))::draw_all_enemies enemy_list (i+1)
    else []
;;

let draw_world state =
  clear_graph ();
  fill_rect (fst state.player) (snd state.player) 50 25;
  let l = (draw_all_enemies state.enemies 0) in
    List.iter (fun i -> Thread.join i) l
  
;;

let draw_player (x,y) = 
  set_color white;
  fill_rect 0 0 1000 100;
  set_color black;
  fill_rect x y 50 25;
;;

let enemy_far_right enemy_list =
  let list = List.sort (fun (x,y) (x2,y2) -> compare x x2) enemy_list in
    List.hd (List.rev list)
;;

let update_enemies enemy_list  =

    List.map (if fst(enemy_far_right enemy_list) >= 800 
                              then  (fun (x,y) -> ((x-200) , y-70)) 
                                
                              else (fun (x,y) ->((x+50), y)) ) enemy_list

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
  
  draw_player state.player;

  let new_time = get_time_now () in
  let update = (new_time -. old_time) > speed_enemy in
    let time' = if update then new_time else old_time in
    let state' = if update then
                  update_state state 
                else
                  state in

    if update then (draw_world state);

  let event = Graphics.wait_next_event [ Graphics.Poll ] in
    if event.Graphics.keypressed then 
      match (read_key ()) with
      |'a' -> handler { state' with player = ((fst state'.player) - 20, snd state'.player) } time'
      |'d' -> handler { state' with player = ((fst state'.player) + 20, snd state'.player) } time'
      | _ -> handler state' time'
    else
      handler state' time'
;;

loop_at_exit [Key_pressed ; Button_down]
   (fun event ->
      handler { 
          player = (300,25);
          enemies = build_enemies enemies_row enemies_line;
          bullets = [ (150, 150) ]
        } 0.0)
;;
