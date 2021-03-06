open Graphics;;
open Printf;;
open StdLabels;;

open_graph " 900x600";;

let get_time_now () = 
  Unix.time ()
;;

let draw_world ~player:(x,y) ~enemy:enemy_list =
  clear_graph ();
  fill_rect x y 100 100;
  List.iter (fun (x2,y2) -> fill_rect x2 y2 50 50) enemy_list;
;;

let update_enemys ~enemy:enemy_list old_time new_time =
  if (new_time -. old_time) > 0.1 then
    List.map (fun (x,y) -> ((x+20) , y) ) enemy_list
  else
    enemy_list
;;

let rec handler ~player:(x,y) ~enemy:enemy_list old_time =
  
  draw_world ~player:(x,y) ~enemy:enemy_list;

  let new_time = get_time_now () in
    let enemy' = update_enemys ~enemy:enemy_list old_time new_time in
    let time' = if (new_time -. old_time) > 0.1 then new_time else old_time in

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
      handler ~player:(300,100) ~enemy:[(50,500);(120,500)] 0.0)
;;
