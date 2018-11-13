open Types;;
open Config;;
open Graphics;;





let  main () =
  
  open_graph " 900x600";
  
  let pl = new player {x = 50; y = 50} in
  set_color black;

  while true do
  let event = Graphics.wait_next_event [ Graphics.Poll ] in
      if event.Graphics.keypressed then
        pl#setKey (read_key ())
      else 
        pl#setKey '0'
  ;
  pl#update;
  pl#draw;
  done
  
;;

  
let _ = main ()