open Config;;
open Graphics;;
open Thread;;

(*** DRAWING FUNCTIONS***)

let draw_enemy (x,y) =
  set_color green;
  fill_rect x y (fst _enemy_size) (snd _enemy_size);
  set_color black;
  fill_rect (x+5) (y+15) 6 6;
  fill_rect (x+17) (y+15) 6 6;
  fill_rect (x) y 2 6;
  fill_rect (x+25) y 2 6;
  fill_rect (x+7) y 4 6;
  fill_rect (x+16) y 4 6;
;;

let rec draw_all_enemies enemy_list =
  List.iter (fun i -> Thread.join i)
    (List.map (fun e -> Thread.create draw_enemy e ) enemy_list)   
;;
let draw_player (x,y) hit =
  if hit then set_color red
  else set_color white;
  fill_rect x y (fst _player_size) (snd _player_size);
  set_color black;
  fill_rect x (y+12) 15 13;
  fill_rect (x + ((fst _player_size) - 15)) (y+12) 15 13;
;;

let draw_bullet (x,y) = 
  fill_rect x y (fst _bullet_size) (snd _bullet_size);
;;

let draw_enemy_bullets bullet_list = 
  set_color red;
  List.iter (fun cord -> draw_bullet cord ) bullet_list
;;

let draw_life life = 
  set_color white;
  moveto 830 5;
  draw_string "LIFES : " ;
  draw_string (string_of_int life);
;;



let draw_world state =
  auto_synchronize false;
  clear_graph ();
  set_color black;
  fill_rect 0 0 900 600;
  set_color (rgb 201 140 0);
  fill_rect (snd _player_boundaries) (snd _initial_player_pos) 10 10;
  fill_rect ((fst _player_boundaries)-10)  (snd _initial_player_pos) 10 10;
  moveto 0 _game_over_line;
  set_color cyan;
  lineto 900 _game_over_line; 
  draw_life state.player_life;
  draw_player state.player state.hit_flash;
  set_color green;
  draw_all_enemies state.enemies;
  set_color (rgb 125 125 125);
  if state.bullet_on then fill_rect (fst state.bullet) (snd state.bullet) (fst _bullet_size) (snd _bullet_size) ;
  draw_enemy_bullets state.enemy_bullets;
  
  synchronize ();
;;

let draw_initial_screen () =
  auto_synchronize false;
  clear_graph ();
  set_color black;
  fill_rect 0 0 900 600;
  moveto 390 300;
  set_color white;
  draw_string "PRESS ANY KEY TO START";
  synchronize ();
;;

let draw_game_over_screen () =
  auto_synchronize false;
  clear_graph ();
  set_color red;
  fill_rect 0 0 900 600;
  moveto 360 300;
  set_color black;
  draw_string "GAME OVER PRESS ESC TO EXIT";
  synchronize ();
;;


let draw_win_screen () =  
    auto_synchronize false;
    clear_graph ();
    set_color cyan;
    fill_rect 0 0 900 600;
    moveto 300 300;
    set_color black;
    draw_string "YOU SAVE US ALL, YOU ARE THE NEW JESUS! PRESS ESC TO EXIT";
    synchronize ();
;;
