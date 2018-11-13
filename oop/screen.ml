open Graphics;;
open Config;;

class screen = object(self)
  
  method openW =  open_graph " 900x600"
  method closeW = close_graph ();

  val mutable state = 0 (*0=initial; 1= In game; 2=lose; 3=win*)
  
  method setState newState = state <- newState 

  method private drawInitial () = 
    auto_synchronize false;
    clear_graph ();
    set_color black;
    fill_rect 0 0 900 600;
    moveto 360 300;
    set_color white;
    draw_string "SAVE US! PRESS ANY KEY TO START";
    synchronize ()

  method private drawInGame () = 
    clear_graph ();
    set_color black;
    fill_rect 0 0 900 600;
    set_color (rgb 201 140 0);
    fill_rect (snd _player_boundaries) (snd _initial_player_pos) 10 10;
    fill_rect ((fst _player_boundaries)-10)  (snd _initial_player_pos) 10 10;
    moveto 0 _game_over_line;
    set_color cyan;
    lineto 900 _game_over_line; 

  method private drawLose () =
    auto_synchronize false;
    clear_graph ();
    set_color red;
    fill_rect 0 0 900 600;
    moveto 300 300;
    set_color black;
    draw_string "WE TRUSTED YOU, AND YOU FAILED. PRESS ESC TO EXIT";
    synchronize ();

  method private drawWin () =  
    auto_synchronize false;
    clear_graph ();
    set_color cyan;
    fill_rect 0 0 900 600;
    moveto 300 300;
    set_color black;
    draw_string "YOU SAVE US ALL, YOU ARE THE NEW JESUS! PRESS ESC TO EXIT";
    synchronize ();


  method draw =
    match state with
    | 0 -> self#drawInitial ()
    | 1 -> self#drawInGame ()
    | 2 -> self#drawLose ()
    | 3 -> self#drawWin ()
    | _ -> ()



end