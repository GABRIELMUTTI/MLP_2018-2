open Graphics;;
open Config;;

class screen = object(self)
  
  val mutable state = 0 (*0=initial; 1= In game; 2=lose; 3=win*)
  method openW =  open_graph " 900x600"


  method private drawInitial () = 
    auto_synchronize false;
  clear_graph ();
  set_color black;
  fill_rect 0 0 900 600;
  moveto 390 300;
  set_color white;
  draw_string "PRESS ANY KEY TO START";
  synchronize ()

  method private drawInGame () = ()
  method private drawLose () = ()
  method private drawWin () =  ()
  method draw =
    match state with
    | 0 -> self#drawInitial ()
    | 1 -> self#drawInGame ()
    | 2 -> self#drawLose ()
    | 3 -> self#drawWin ()
    | _ -> ()



end