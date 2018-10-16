open Graphics;;
open Printf;;
open StdLabels;;
open Thread;;
let enemys_line = 4;;
let enemys_row = 8;;
let speed_enemy = 0.5;;
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

let draw_enemy (x,y) =
  fill_rect x y 25 25;
;;

let rec draw_all_enemys enemy_list i =
  if i < (List.length enemy_list) then
    (Thread.create draw_enemy (List.nth enemy_list i))::draw_all_enemys enemy_list (i+1)
    else []
;;

let draw_world ~player:(x,y) ~enemy:enemy_list =
  clear_graph ();
  fill_rect x y 50 25;
  let l = (draw_all_enemys enemy_list 0) in
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

let update_enemys ~enemy:enemy_list update =
  if update then
    List.map (if fst(enemy_far_right enemy_list) >= 800 
                              then  (fun (x,y) -> ((x-200) , y-70)) 
                                
                              else (fun (x,y) ->((x+50), y))) enemy_list
  else
    enemy_list
;;

let rec handler ~player:(x,y) ~enemy:enemy_list old_time =
  
  draw_player (x,y);

  let new_time = get_time_now () in
  let update = (new_time -. old_time) > speed_enemy in
    let enemy' = update_enemys ~enemy:enemy_list update in
    let time' = if update then new_time else old_time in
    if update then (draw_world ~player:(x,y) ~enemy:enemy_list);

  let event = Graphics.wait_next_event [ Graphics.Poll ] in
    if event.Graphics.keypressed then 
      match (read_key ()) with
      |'a' -> handler ~player:((x-20),y) ~enemy:enemy' time'
      |'d' -> handler ~player:((x+20),y) ~enemy:enemy' time'
      |'q' -> handler ~player:(300,100) ~enemy:enemy' time'
      | _ -> handler ~player:(x,y) ~enemy:enemy' time'
    else
      handler ~player:(x,y) ~enemy:enemy' time'
  
;;

loop_at_exit [Key_pressed ; Button_down]
   (fun event ->
      handler ~player:(300,10) ~enemy:(build_enemys enemys_row enemys_line) 0.0)
;;