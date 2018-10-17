open Config;;
open Graphics;;
open Thread;;

(*** DRAWING FUNCTIONS***)

let draw_enemy (x,y) =
  fill_rect x y (fst _enemy_size) (snd _enemy_size);
;;

let rec draw_all_enemies enemy_list =
  List.iter (fun i -> Thread.join i)
    (List.map (fun e -> Thread.create draw_enemy e ) enemy_list)   
;;

let draw_world state =
  auto_synchronize false;
  clear_graph ();
  set_color black;
  fill_rect 0 0 900 600;
  set_color (rgb 201 140 0);
  fill_rect (snd _player_boundaries) (snd _initial_player_pos) 10 10;
  fill_rect ((fst _player_boundaries)-10)  (snd _initial_player_pos) 10 10;
  set_color white;
  fill_rect (fst state.player) (snd state.player) (fst _player_size) (snd _player_size);
  set_color green;
  draw_all_enemies state.enemies;
  set_color (rgb 125 125 125);
  List.iter (fun (x, y) -> fill_rect x y (fst _bullet_size) (snd _bullet_size)) state.bullets;
  
  synchronize ();
;;
