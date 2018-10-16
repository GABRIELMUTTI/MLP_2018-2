open Graphics;;
open Printf;;
open StdLabels;;

open_graph " 900x600";;

let get_time_now () = 
  Unix.time ()
;;

let draw_world ~player:(x,y) ~enemy:(x2,y2) =
  clear_graph ();
  fill_rect x y 100 100;
  fill_rect x2 y2 100 100;
;;



let rec handler ~player:(x,y) ~enemy:(x2,y2) old_time =
  
  draw_world ~player:(x,y) ~enemy:(x2,y2);

  let new_time = get_time_now () in
    
    if (new_time -. old_time) > 0.05 then 
      handler ~player:(x,y) ~enemy:((x2+10),y2) new_time;
 

  let event = Graphics.wait_next_event [ Graphics.Poll ] in
    if event.Graphics.keypressed then
      match (read_key ()) with
      |'a' -> handler ~player:((x-20),y) ~enemy:(x2,y2) old_time
      |'d' -> handler ~player:((x+20),y) ~enemy:(x2,y2) old_time
      |'w' -> handler ~player:(x,(y+20)) ~enemy:(x2,y2) old_time
      |'s' -> handler ~player:(x,(y-20)) ~enemy:(x2,y2) old_time
      |'q' -> handler ~player:(300,100) ~enemy:(x2,y2) old_time
      | _ -> handler ~player:(x,y) ~enemy:(x2,y2) old_time
    else
      handler ~player:(x,y) ~enemy:(x2,y2) old_time
;;

loop_at_exit [Key_pressed ; Button_down]
   (fun event ->
      handler ~player:(300,100) ~enemy:(100,300) 0.0);;
